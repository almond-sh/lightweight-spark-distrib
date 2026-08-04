//> using lib "com.github.alexarchambault::case-app:2.1.0-M14"
//> using lib "com.lihaoyi::os-lib:0.8.1"
//> using lib "com.lihaoyi::pprint:0.7.3"
//> using lib "io.get-coursier::coursier:2.1.25-M25"
//> using lib "io.get-coursier::coursier-archive-cache:2.1.25-M25"
//> using lib "org.apache.commons:commons-compress:1.21"
//> using scala "2.13.18"
//> using jvm "17"

//> using option "-Ywarn-unused"

import caseapp.core.app.CaseApp
import caseapp.core.RemainingArgs
import coursier.cache.loggers.RefreshLogger
import coursier.cache.{ArchiveCache, FileCache}
import coursier.core.Publication
import coursier.error.ResolutionError
import coursier.util.Artifact
import coursier.{dependencyString => _, _}
import dependency._
import Util._

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal
import scala.util.Properties
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream
import scala.util.Using
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream
import org.apache.commons.compress.archivers.tar.TarArchiveEntry

object Convert extends CaseApp[ConvertOptions] {

  // has to accept input like "hive-cli-2.3.7.jar", but also "scopt_2.12-3.7.1.jar" (Scala version suffix)
  def moduleName(fileName: String): String = {
    val f = fileName.stripSuffix(".jar")
    val lastDashIdx = f.lastIndexOf('-')
    if (lastDashIdx < 0) f
    else {
      val followedByVersion = lastDashIdx.until(f.length).forall { idx =>
        val c = f(idx)
        c.isDigit || c == '.' || c == '-'
      }
      if (followedByVersion)
        moduleName(f.take(lastDashIdx))
      else
        f
    }
  }

  def versionFor(fileName: String): String =
    fileName
      .stripPrefix(moduleName(fileName))
      .stripPrefix("-")
      .stripSuffix(".jar")

  def fetchJarVersion(dep: coursier.Dependency, pub: Publication, forceVersion: String): Option[String] = {
    val dep0 = dep.withVersion(forceVersion).withTransitive(false).withPublication(pub)
    val cache = FileCache().withLogger(RefreshLogger.create())
    val resOpt =
      try Some(cache.logger.use(Fetch().addDependencies(dep0).withCache(cache).runResult()(cache.ec)))
      catch {
        case e: ResolutionError.CantDownloadModule if e.perRepositoryErrors.forall(_.startsWith("not found: ")) =>
          None
        case NonFatal(e) =>
          throw new Exception(e)
      }
    resOpt.map(_.artifacts).getOrElse(Nil) match {
      case Seq() =>
        System.err.println(s"Error: could not get ${dep.module}:$forceVersion (no file found)")
        None
      case Seq((a, _)) =>
        Some(a.url)
      case Seq((a, _), _*) =>
        System.err.println(s"Warning: got several files for ${dep.module}:$forceVersion (should not happen)")
        Some(a.url)
    }
  }

  def csShUrl = "https://github.com/coursier/ci-scripts/raw/e1fe4d7735551826571140c627f504d649b893ca/cs.sh"

  // spark.shade.packageName, from Spark's parent POM (Spark 3.0 renamed it)
  def sparkShadePackage(sparkVersion: String): String =
    if (sparkVersion.startsWith("2.")) "org.spark_project"
    else "org.sparkproject"

  def nettyNativeLibDir = "META-INF/native"

  /** The netty native libraries Spark's build renames, straight from the maven-antrun-plugin
    * execution of common/network-yarn/pom.xml.
    *
    * That list follows the Spark version: it grew as netty did, and only what it names gets
    * renamed, so renaming one that Spark leaves alone is as wrong as leaving one of these be.
    * Spark 2.4 renames none of them at all. The MS Windows DLL is in no list, and wouldn't match
    * anyway, as it doesn't carry the `lib` prefix the renaming keys on.
    */
  def nettyNativeLibs(sparkVersion: String): Seq[String] = {
    val transport = Seq(
      "netty_transport_native_epoll_x86_64.so",
      "netty_transport_native_kqueue_x86_64.jnilib"
    )
    val aarch64 = Seq(
      "netty_transport_native_epoll_aarch_64.so",
      "netty_transport_native_kqueue_aarch_64.jnilib"
    )
    val riscv64AndTcnative = Seq(
      "netty_transport_native_epoll_riscv64.so",
      "netty_tcnative_linux_x86_64.so",
      "netty_tcnative_linux_aarch_64.so",
      "netty_tcnative_osx_x86_64.jnilib",
      "netty_tcnative_osx_aarch_64.jnilib"
    )
    val ioUringAndQuiche = Seq(
      "netty_transport_native_io_uring42_x86_64.so",
      "netty_transport_native_io_uring42_aarch_64.so",
      "netty_transport_native_io_uring42_riscv64.so",
      "netty_quiche42_linux_x86_64.so",
      "netty_quiche42_linux_aarch_64.so",
      "netty_quiche42_osx_x86_64.jnilib",
      "netty_quiche42_osx_aarch_64.jnilib"
    )
    if (sparkVersion.startsWith("2."))
      Nil
    else if (sparkVersion.startsWith("3.0.") || sparkVersion.startsWith("3.1.") || sparkVersion.startsWith("3.2."))
      transport
    else if (sparkVersion.startsWith("3."))
      transport ++ aarch64
    else if (sparkVersion.startsWith("4.1."))
      transport ++ aarch64 ++ riscv64AndTcnative ++ ioUringAndQuiche
    else
      transport ++ aarch64 ++ riscv64AndTcnative
  }

  def shadedNettyNativeLibPrefix(sparkVersion: String): String =
    s"lib${sparkShadePackage(sparkVersion).replace('.', '_')}_"

  def nettyNativeLibRenames(sparkVersion: String): Map[String, String] =
    nettyNativeLibs(sparkVersion)
      .map { lib =>
        s"$nettyNativeLibDir/lib$lib" -> s"$nettyNativeLibDir/${shadedNettyNativeLibPrefix(sparkVersion)}$lib"
      }
      .toMap

  /** Renames the netty native libraries of a JAR we generated, the way Spark's build does with a
    * maven-antrun-plugin execution that runs after the shading.
    *
    * The relocated netty derives the name of the libraries it loads from its own package, so
    * without this it finds none of them, and silently falls back to NIO and to the JDK SSL
    * provider.
    */
  def renameNettyNativeLibs(jar: os.Path, sparkVersion: String): Unit = {
    val renames = nettyNativeLibRenames(sparkVersion)
    val updated = os.temp(prefix = jar.last.stripSuffix(".jar"), suffix = ".jar")
    Using.resource(new ZipFile(jar.toIO)) { zf =>
      Using.resource(new ZipOutputStream(Files.newOutputStream(updated.toNIO))) { zos =>
        for (ent <- zf.entries.asScala) {
          val updatedEnt = new ZipEntry(renames.getOrElse(ent.getName, ent.getName))
          updatedEnt.setTime(ent.getTime)
          zos.putNextEntry(updatedEnt)
          if (!ent.isDirectory)
            Using.resource(zf.getInputStream(ent))(_.transferTo(zos))
          zos.closeEntry()
        }
      }
    }
    os.move(updated, jar, replaceExisting = true)
  }

  /** The same renaming, as a shell snippet, for the script the lightweight distribution ships.
    *
    * Unzipping and re-jarring is what Spark's own build does here. It needs `jar`, which a JRE
    * doesn't have, so fall back on a JVM coursier gives us rather than on whatever runs Spark.
    */
  def renameNettyNativeLibsScript(dest: String, sparkVersion: String): String = {
    val libs = nettyNativeLibs(sparkVersion)
    if (libs.isEmpty) ""
    else {
      val patterns = libs.map(lib => shellQuote(s"$nettyNativeLibDir/lib$lib")).mkString(" ")
      s"""
         |# Spark's build renames these after the shading, so that the relocated netty finds them
         |shuffle_jar="$$PWD/${shellQuote(dest)}"
         |if unzip -l "$$shuffle_jar" $patterns > /dev/null 2>&1; then
         |  echo "Renaming netty native libraries in $dest" 1>&2
         |  if [ -n "$${JAVA_HOME:-}" ] && [ -x "$$JAVA_HOME/bin/jar" ]; then
         |    jar_command="$$JAVA_HOME/bin/jar"
         |  elif command -v jar > /dev/null 2>&1; then
         |    jar_command="jar"
         |  else
         |    jar_command="$$(./fetch-jars/cs.sh java-home)/bin/jar"
         |  fi
         |  exploded="$$(mktemp -d)"
         |  unzip -q "$$shuffle_jar" -d "$$exploded"
         |  (
         |    cd "$$exploded/$nettyNativeLibDir"
         |    for f in ${libs.map(shellQuote).mkString(" ")}; do
         |      if [ -f "lib$$f" ]; then
         |        mv "lib$$f" "${shadedNettyNativeLibPrefix(sparkVersion)}$$f"
         |      fi
         |    done
         |  )
         |  "$$jar_command" --create --no-manifest --file "$$shuffle_jar" -C "$$exploded" .
         |  rm -rf "$$exploded"
         |fi
         |""".stripMargin
    }
  }

  def yarnShuffleJarName(sparkVersion: String): String =
    s"spark-$sparkVersion-yarn-shuffle.jar"

  /** The coursier command that re-creates yarn/spark-$sparkVersion-yarn-shuffle.jar.
    *
    * Spark builds that JAR with the maven-shade-plugin, out of common/network-yarn. The POM
    * published for that module is a dependency-reduced one -- everything the shade plugin bundled
    * was stripped from it -- so spark-network-shuffle, its only compile dependency in the source
    * POM, has to be asked for explicitly. The exclusions stand for the `provided` scopes of that
    * POM and of the Spark parent POM, and for what the shade plugin's artifactSet leaves out;
    * guava and org.spark-project.spark:unused need no adding, as they are already relocated inside
    * the published spark-network-common JAR, and netty-common ships jctools pre-shaded. The
    * relocations and the excluded entries are the shade plugin configuration of that same POM.
    *
    * `shadedHadoopClient` tells whether the distribution was built against the shaded
    * hadoop-client-api / hadoop-client-runtime rather than the plain hadoop-client. Hadoop is
    * `provided` either way, but a plain hadoop-client still drags its own dependency tree into the
    * resolution Maven does for the module, and two of its members end up in the JAR.
    *
    * Arguments are grouped so that each group can go on a line of its own when the command is
    * written to a script.
    */
  def yarnShuffleJarCommand(
    sparkVersion: String,
    scalaBinaryVersion: String,
    shadedHadoopClient: Boolean,
    dest: String
  ): Seq[Seq[String]] = {

    val shadePackage = sparkShadePackage(sparkVersion)

    // up to Spark 3.1, the parent POM manages paranamer -- which Hadoop pulls via Avro -- to the
    // runtime scope, which is enough for the shade plugin to bundle it even though the
    // hadoop-client it comes from is provided
    val maybeParanamer =
      if (shadedHadoopClient) Nil
      else if (sparkVersion.startsWith("2.") || sparkVersion.startsWith("3.0.") || sparkVersion.startsWith("3.1."))
        Seq(Seq("com.thoughtworks.paranamer:paranamer:2.8"))
      else Nil

    // the shade plugin's artifactSet leaves scala-library out, until Spark 4.0 where
    // spark-network-common stopped depending on any Scala code at all
    val maybeScalaLibrary =
      if (sparkVersion.startsWith("4.")) Nil
      else Seq(Seq("-E", "org.scala-lang:scala-library"))

    // Tink asks for a much newer gson, but a plain hadoop-client puts its own, older one nearer in
    // the graph, and Maven goes for that one
    val maybeGson =
      if (shadedHadoopClient) Nil
      else Seq(Seq("-V", "com.google.code.gson:gson:2.2.4"))

    // netty's QUIC codec, which Spark 4.1 pulls in, is the only thing asking for bouncycastle here,
    // and the parent POM manages it to the test scope
    val maybeBouncyCastle =
      if (sparkVersion.startsWith("4.")) Seq(Seq("-E", "org.bouncycastle:bcprov-jdk18on"))
      else Nil

    // only Spark 4.0 onwards filters META-INF/LICENSE out
    val maybeLicense =
      if (sparkVersion.startsWith("4.")) Seq(Seq("-R", "exclude:META-INF/LICENSE"))
      else Nil

    Seq(
      Seq("bootstrap", "--assembly", "--no-main-class", "-f"),
      Seq("-o", dest),
      Seq(s"org.apache.spark:spark-network-yarn_$scalaBinaryVersion:$sparkVersion"),
      Seq(s"org.apache.spark:spark-network-shuffle_$scalaBinaryVersion:$sparkVersion")
    ) ++ maybeParanamer ++ Seq(
      Seq("-E", s"org.apache.spark:spark-tags_$scalaBinaryVersion"),
      Seq("-E", "com.google.protobuf:protobuf-java"),
      Seq("-E", "org.slf4j:slf4j-api")
    ) ++ maybeScalaLibrary ++ maybeGson ++ maybeBouncyCastle ++ Seq(
      Seq("--relocate", s"com.fasterxml.jackson=$shadePackage.com.fasterxml.jackson"),
      Seq("--relocate", s"io.netty=$shadePackage.io.netty"),
      Seq("-R", "exclude:META-INF/INDEX.LIST"),
      Seq("-R", "exclude:module-info.class")
    ) ++ maybeLicense
  }

  def shellQuote(arg: String): String =
    if (arg.nonEmpty && arg.forall(c => c.isLetterOrDigit || "._:/=@+-".contains(c))) arg
    else "'" + arg.replace("'", "'\\''") + "'"

  /** Whether the maven-shade-plugin Spark builds with renames the META-INF/services entries of the
    * classes it relocates. Spark 3.3 is the first release whose plugin version does.
    */
  def relocatesServices(sparkVersion: String): Boolean =
    !(sparkVersion.startsWith("2.") || sparkVersion.startsWith("3.0.") ||
      sparkVersion.startsWith("3.1.") || sparkVersion.startsWith("3.2."))

  /** Rewrites an entry name of a JAR we generated to the name Spark's own build would give it.
    *
    * Two differences are left once the netty native libraries have been renamed, both of them
    * jarjar -- which coursier shades with -- doing a more thorough job than the maven-shade-plugin:
    *   - the shade plugin relocates the *content* of multi-release entries, under
    *     META-INF/versions, but leaves their *path* alone, where jarjar relocates both;
    *   - the shade plugin version Spark used before 3.3 leaves the META-INF/services entry of a
    *     relocated service alone too.
    */
  def normalizeYarnShuffleJarEntry(sparkVersion: String)(name: String): String = {
    val shadePackage = sparkShadePackage(sparkVersion)
    val versionsPrefix = "META-INF/versions/"
    val servicesPrefix = s"META-INF/services/$shadePackage."
    if (name.startsWith(versionsPrefix)) {
      val rest = name.stripPrefix(versionsPrefix)
      val idx = rest.indexOf('/')
      val relocatedPrefix = shadePackage.replace('.', '/') + "/"
      if (idx < 0 || !rest.drop(idx + 1).startsWith(relocatedPrefix)) name
      else versionsPrefix + rest.take(idx + 1) + rest.drop(idx + 1).stripPrefix(relocatedPrefix)
    }
    else if (!relocatesServices(sparkVersion) && name.startsWith(servicesPrefix))
      "META-INF/services/" + name.stripPrefix(servicesPrefix)
    else name
  }

  /** Compares the entry names of a JAR we generated with those of the one Spark ships.
    *
    * Directory entries are left out: Spark's JAR is unzipped and re-jarred by the antrun step
    * above, which writes an entry for every directory, where an assembly only carries the ones its
    * inputs had.
    */
  def compareYarnShuffleJarEntries(generated: os.Path, reference: os.Path, sparkVersion: String): Boolean = {
    def entries(jar: os.Path, normalize: Boolean) =
      Using.resource(new ZipFile(jar.toIO)) { zf =>
        zf.entries
          .asScala
          .map(_.getName)
          .filter(!_.endsWith("/"))
          .map(name => if (normalize) normalizeYarnShuffleJarEntry(sparkVersion)(name) else name)
          .toSet
      }

    val expected = entries(reference, normalize = false)
    val got = entries(generated, normalize = true)

    def report(label: String, names: Set[String]): Unit =
      if (names.nonEmpty) {
        System.err.println(s"Error: ${names.size} $label entries in ${generated.last}:")
        for (name <- names.toVector.sorted.take(20))
          System.err.println(s"  $name")
        if (names.size > 20)
          System.err.println(s"  … and ${names.size - 20} more")
      }

    report("extraneous", got -- expected)
    report("missing", expected -- got)

    val ok = got == expected
    if (ok)
      System.err.println(s"${generated.last} has the same ${got.size} entries as ${reference}")
    ok
  }

  def run(options: ConvertOptions, args: RemainingArgs): Unit = {

    val arg = args.all match {
      case Seq() =>
        System.err.println("Error: no argument passed (expected Spark distribution path or URL)")
        System.err.println(finalHelp.usage(helpFormat))
        sys.exit(1)
      case Seq(arg) => arg
      case _ =>
        System.err.println("Error: too many arguments passed (expected one)")
        System.err.println(finalHelp.usage(helpFormat))
        sys.exit(1)
    }

    val distribPath =
      if (arg.contains("://")) {
        val cache = FileCache().withLogger(RefreshLogger.create())
        val archiveCache = ArchiveCache().withCache(cache)
        val artifact = Artifact(arg).withChanging(options.changing)
        cache.logger.using(archiveCache.get(artifact)).unsafeRun(true)(cache.ec) match {
          case Left(e)  => throw new Exception(e)
          case Right(f) => os.Path(f, os.pwd)
        }
      }
      else
        os.Path(arg, os.pwd)

    val dest = os.Path(options.dest, os.pwd)

    if (os.exists(dest)) {
      if (options.force) {
        System.err.println(s"${options.dest} already exists, removing it…")
        os.remove.all(dest)
      }
      else {
        System.err.println(s"${options.dest} already exists, pass --force to force removing it.")
        sys.exit(1)
      }
    }

    val dirDest =
      if (options.archive) os.temp.dir(prefix = "convert-spark-distrib")
      else dest

    def size(p: os.Path): Long =
      if (os.isDir(p)) os.list(p).map(size).sum
      else os.size(p)

    System.err.println(s"Input Spark archive: $distribPath")
    System.err.println(s"Size: ${size(distribPath)} B")

    convert(
      distribPath,
      dirDest,
      options.scalaVersion,
      options.sparkVersion,
      options.checkYarnShuffleJar,
      options.csCommand
    )

    if (options.archive) {
      Using.resource(os.write.outputStream(dest, createFolders = true)) { fos =>
        Using.resource(new GzipCompressorOutputStream(fos)) { gzos =>
          Using.resource(new TarArchiveOutputStream(gzos)) { taos =>
            taos.setLongFileMode(TarArchiveOutputStream.LONGFILE_GNU)
            for (p <- os.walk.stream(dirDest) if p != dirDest) {
              val relPath = p.relativeTo(dirDest)
              val ent = new TarArchiveEntry(p.toNIO, relPath.toString)
              if (!Properties.isWin) {
                val perms = os.perms(p)
                ent.setMode(perms.toInt)
              }
              taos.putArchiveEntry(ent)
              if (os.isFile(p))
                taos.write(os.read.bytes(p))
              taos.closeArchiveEntry()
            }
          }
        }
      }
    }

    System.err.println(s"Output Spark distribution: $dest")
    System.err.println(s"Size: ${size(dest)} B")
  }

  def convert(
    distribPath: os.Path,
    dest: os.Path,
    scalaVersionOpt: Option[String],
    sparkVersionOpt: Option[String],
    checkYarnShuffleJar: Boolean = false,
    csCommand: List[String] = Nil
  ): Unit = {

    os.makeDir.all(dest)

    val sparkVersion = sparkVersionOpt.map(_.trim).filter(_.nonEmpty).getOrElse {
      def findSparkJar(name: String) =
        os.walk.stream(distribPath).find { p =>
          p.segments.contains("jars") &&
          p.last.startsWith(s"spark-${name}_") &&
          p.last.endsWith(".jar") &&
          os.isFile(p)
        }

      val sparkPropertiesJar = findSparkJar("common-utils")
        .orElse(findSparkJar("core"))
        .getOrElse {
          System.err.println(s"Error: could not find a **/jars/spark-{core,common-utils}_*.jar file under $distribPath")
          sys.exit(1)
        }

      val versionLinePrefix = "version="
      val versionOpt = Using.resource(new ZipFile(sparkPropertiesJar.toIO)) { zf =>
        Option(zf.getEntry("spark-version-info.properties")).flatMap { ent =>
          Using.resource(zf.getInputStream(ent)) { is =>
            new String(is.readAllBytes(), StandardCharsets.UTF_8)
              .linesIterator
              .map(_.trim)
              .find(_.startsWith(versionLinePrefix))
              .map(_.stripPrefix(versionLinePrefix).trim)
          }
        }
      }

      versionOpt.getOrElse {
        System.err.println(s"Error: could not find a $versionLinePrefix line in spark-version-info.properties inside $sparkPropertiesJar")
        sys.exit(1)
      }
    }
    val scalaVersion = scalaVersionOpt.map(_.trim).filter(_.nonEmpty).getOrElse {
      val scalaLibraryJar = os.walk.stream(distribPath)
        .find { p =>
          p.segments.contains("jars") &&
          p.last.startsWith("scala-library-") &&
          p.last.endsWith(".jar") &&
          os.isFile(p)
        }
        .getOrElse {
          System.err.println(s"Error: could not find a **/jars/scala-library-*.jar file under $distribPath")
          sys.exit(1)
        }

      val versionLinePrefix = "version.number="
      val versionOpt = Using.resource(new ZipFile(scalaLibraryJar.toIO)) { zf =>
        Option(zf.getEntry("library.properties")).flatMap { ent =>
          Using.resource(zf.getInputStream(ent)) { is =>
            new String(is.readAllBytes(), StandardCharsets.UTF_8)
              .linesIterator
              .map(_.trim)
              .find(_.startsWith(versionLinePrefix))
              .map(_.stripPrefix(versionLinePrefix).trim)
          }
        }
      }

      versionOpt.getOrElse {
        System.err.println(s"Error: could not find a $versionLinePrefix line in library.properties inside $scalaLibraryJar")
        sys.exit(1)
      }
    }

    // Ammonite JARs are only in some distributions, and are cross-published for the full Scala version,
    // like jars/ammonite_2.13.18-3.0.8.jar (ammonite-*.jar files are only cross-published for the Scala
    // binary version, and don't give us the full Ammonite version)
    val ammoniteVersionOpt = {
      val prefix = "ammonite_"
      os.walk.stream(distribPath)
        .find { p =>
          p.segments.contains("jars") &&
          p.last.startsWith(prefix) &&
          p.last.endsWith(".jar") &&
          os.isFile(p)
        }
        .flatMap { p =>
          // strips the Scala version, like "2.13.18-3.0.8" -> "3.0.8"
          val versions = p.last.stripPrefix(prefix).stripSuffix(".jar")
          val dashIdx = versions.indexOf('-')
          if (dashIdx < 0) {
            System.err.println(s"Warning: could not get an Ammonite version from ${p.last}, ignoring it")
            None
          }
          else
            Some(versions.drop(dashIdx + 1))
        }
    }
    for (ammoniteVersion <- ammoniteVersionOpt)
      System.err.println(s"Found Ammonite $ammoniteVersion in $distribPath")

    // FIXME Add more?
    // (see "cs complete-dependency org.apache.spark: | grep '_2\.12$'"
    // or `ls "$(cs get https://archive.apache.org/dist/spark/spark-2.4.2/spark-2.4.2-bin-hadoop2.7.tgz --archive)"/*/jars | grep '^spark-'`)
    def sparkModules(sparkVersion: String) = {
      val maybeMesos =
        if (sparkVersion.startsWith("3.") || sparkVersion.startsWith("2.4."))
          Seq("mesos")
        else
          Nil
      val maybeConnect =
        if (sparkVersion.startsWith("3.4.") || sparkVersion.startsWith("3.5.") || sparkVersion.startsWith("4."))
          Seq("connect", "connect-client-jvm")
        else
          Nil
      val maybeConnectJdbc =
        if (sparkVersion.startsWith("4.") && !sparkVersion.startsWith("4.0."))
          Seq("connect-client-jdbc")
        else
          Nil
      Seq(
        "core",
        "graphx",
        "hive",
        "hive-thriftserver",
        "kubernetes",
        "mllib",
        "repl",
        "sql",
        "streaming",
        "yarn"
      ) ++ maybeMesos ++ maybeConnect ++ maybeConnectJdbc
    }

    // Spark's network-yarn module depends on the shaded hadoop-client-api / hadoop-client-runtime
    // from Spark 3.2 on, but the Hadoop 2 profiles map those back to the plain hadoop-client. The
    // distribution ships whichever of the two the build resolved.
    val shadedHadoopClient = os.walk.stream(distribPath).exists { p =>
      p.segments.contains("jars") &&
      p.last.startsWith("hadoop-client-api-") &&
      p.last.endsWith(".jar") &&
      os.isFile(p)
    }
    System.err.println(
      if (shadedHadoopClient) "Distribution built against the shaded Hadoop client"
      else "Distribution built against the plain Hadoop client"
    )

    val params = ScalaParameters(scalaVersion)
    val sparkDependencies = sparkModules(sparkVersion).map { mod =>
      dep"org.apache.spark::spark-$mod:$sparkVersion".applyParams(params).toCs
    }
    val extraDependencies = Seq(
      dep"com.github.scopt::scopt:3.7.1".applyParams(params).toCs
    ) ++ ammoniteVersionOpt.map { ammoniteVersion =>
      dep"com.lihaoyi:::ammonite:$ammoniteVersion,exclude=com.google.code.gson%gson".applyParams(params).toCs
    }
    val dependencies = sparkDependencies ++ extraDependencies

    System.err.println(s"Fetching Spark JARs via coursier")
    val cache = FileCache().withLogger(RefreshLogger.create())
    val res =
      try
        cache.logger.use {
         Fetch()
            .addDependencies(dependencies: _*)
            .withCache(cache)
            .runResult()(cache.ec)
        }
      catch {
        case NonFatal(e) =>
          throw new Exception(e)
      }
    res.files
    System.err.println(s"Got ${res.files.length} JARs")

    val map = res
      .artifacts
      .map {
        case (a, f) =>
          f.getName -> a.url
      }
      .toMap

    val moduleNameMap = res
      .fullDetailedArtifacts
      .collect {
        case (d, p, a, Some(f)) =>
          (moduleName(f.getName), (d, p, a.url))
      }
      .toMap

    val entries = new mutable.ListBuffer[(String, os.SubPath)]
    // JARs the generated script builds with coursier rather than downloads, as (source, destination)
    val generatedJars = new mutable.ListBuffer[(os.Path, os.SubPath)]

    // The YARN shuffle service JAR is a ~100 MB assembly that isn't published anywhere, and weighs
    // more than the rest of the distribution put together. It can be re-created from JARs that
    // *are* on Maven Central, so leave it out and have fetch-jars.sh build it.
    def isYarnShuffleJar(rel: os.SubPath) =
      rel.segments.takeRight(2).toSeq == Seq("yarn", yarnShuffleJarName(sparkVersion))

    for (p <- os.walk.stream(distribPath)) {
      val rel = p.relativeTo(distribPath).asSubPath
      if (os.isDir(p))
        os.makeDir(dest / rel)
      else if (isYarnShuffleJar(rel))
        generatedJars += p -> rel
      else if (rel.last.endsWith(".jar")) {
        val urlOpt = map.get(rel.last).orElse {
          moduleNameMap.get(moduleName(rel.last)).flatMap {
            case (d, p, _) =>
              fetchJarVersion(d, p, versionFor(rel.last))
          }
        }

        urlOpt match {
          case None =>
            System.err.println(s"Warning: $rel (${moduleName(rel.last)}) not found")
            // Is copyAttributes fine on Windows?
            os.copy(p, dest / rel, copyAttributes = true)
          case Some(url) =>
            entries += url -> rel
        }
      }
      else
        // Is copyAttributes fine on Windows?
        os.copy(p, dest / rel, copyAttributes = true)
    }

    val csSh = cache.logger.using(cache.file(Artifact(csShUrl)).run).unsafeRun(true)(cache.ec) match {
      case Left(e) => throw new Exception(e)
      case Right(f) => os.Path(f, os.pwd)
    }
    val csShContent = os.read(csSh)
      .linesIterator
      .map { line =>
        if (line.startsWith("CS_VERSION=")) """CS_VERSION="nightly""""
        else line
      }
      .mkString("\n") + "\n"

    val (destBase, dropHead) = os.list(dest) match {
      case Seq(dir) if os.isDir(dir) => (dir, true)
      case _ => (dest, false)
    }
    val fetchJarsDir = destBase / "fetch-jars"
    os.makeDir.all(fetchJarsDir)
    os.write(fetchJarsDir / "cs.sh", csShContent, perms = if (Properties.isWin) null else "rwxr-xr-x")

    def relativeToBase(rel: os.SubPath) =
      if (dropHead) rel.segments.drop(1).mkString("/") else rel.toString

    val generatedJarCommands = generatedJars
      .toList
      .map {
        case (_, rel) =>
          val dest0 = relativeToBase(rel)
          dest0 -> yarnShuffleJarCommand(sparkVersion, params.scalaBinaryVersion, shadedHadoopClient, dest0)
      }

    def fetchJarContent = {
      val generate = generatedJarCommands
        .map {
          case (dest0, command) =>
            val commandStr = command
              .map(_.map(shellQuote).mkString(" "))
              .mkString("./fetch-jars/cs.sh ", " \\\n  ", "")
            s"""
               |echo "Generating $dest0" 1>&2
               |$commandStr
               |${renameNettyNativeLibsScript(dest0, sparkVersion)}""".stripMargin
        }
        .mkString
      s"""#!/usr/bin/env bash
         |set -eu
         |
         |cd "$$(dirname "$${BASH_SOURCE[0]}")"
         |
         |cat fetch-jars/jar-urls | while read entry; do
         |  url="$$(echo "$$entry" | sed 's/->.*$$//')"
         |  dest="$$(echo "$$entry" | sed 's/^.*->//')"
         |  echo "Getting $$dest from $$url" 1>&2
         |  cp "$$(./fetch-jars/cs.sh get "$$url")" "$$dest"
         |done
         |$generate""".stripMargin.getBytes(StandardCharsets.UTF_8)
    }
    os.write(destBase / "fetch-jars.sh", fetchJarContent, perms = if (Properties.isWin) null else "rwxr-xr-x")

    for ((source, rel) <- generatedJars)
      System.err.println(s"$rel left out, fetch-jars.sh will re-create it from ${source.last}'s dependencies")

    if (checkYarnShuffleJar)
      for ((source, rel) <- generatedJars) {
        val command = if (csCommand.isEmpty) Seq((fetchJarsDir / "cs.sh").toString) else csCommand
        val generated = os.temp(prefix = rel.last.stripSuffix(".jar"), suffix = ".jar")
        System.err.println(s"Re-creating $rel to compare it with the one $distribPath ships")
        val command0 =
          yarnShuffleJarCommand(sparkVersion, params.scalaBinaryVersion, shadedHadoopClient, generated.toString)
        os.proc(command ++ command0.flatten)
          .call(cwd = os.pwd, stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)
        renameNettyNativeLibs(generated, sparkVersion)
        val ok = compareYarnShuffleJarEntries(generated, source, sparkVersion)
        os.remove(generated)
        if (!ok) {
          System.err.println(s"Error: entry names of re-created $rel do not match the one shipped with Spark")
          sys.exit(1)
        }
      }

    def entriesContent =
      entries
        .toList
        .map {
          case (url, dest) =>
            s"$url->${relativeToBase(dest)}${System.lineSeparator()}"
        }
        .mkString
        .getBytes(StandardCharsets.UTF_8)
    os.write(fetchJarsDir / "jar-urls", entriesContent)
  }
}
