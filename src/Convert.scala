//> using lib "com.github.alexarchambault::case-app:2.1.0-M14"
//> using lib "com.lihaoyi::os-lib:0.8.1"
//> using lib "com.lihaoyi::pprint:0.7.3"
//> using lib "io.get-coursier::coursier:2.1.0-M5-24-g678b31710"
//> using lib "io.get-coursier::dependency:0.2.2"
//> using lib "org.apache.commons:commons-compress:1.21"
//> using scala "2.13.8"

//> using option "-Ywarn-unused"

import caseapp.core.app.CaseApp
import caseapp.core.RemainingArgs
import coursier.cache.{ArchiveCache, FileCache}
import coursier.core.Publication
import coursier.error.ResolutionError
import coursier.util.Artifact
import coursier.{dependencyString => _, _}
import dependency._
import Util._

import java.nio.charset.StandardCharsets
import java.util.zip.ZipFile

import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.Properties
import org.apache.commons.compress.compressors.gzip.GzipCompressorOutputStream
import scala.util.Using
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream
import org.apache.commons.compress.archivers.tar.TarArchiveEntry
import coursier.cache.Cache
import coursier.cache.CacheLogger
import coursier.cache.loggers.RefreshLogger

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
    val resOpt =
      try Some(Fetch().addDependencies(dep0).runResult()(FileCache().ec))
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

  def csShUrl = "https://github.com/coursier/ci-scripts/raw/dcc000482233f5d4194b11e36573862a869b2fd7/cs.sh"

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
        archiveCache.get(artifact).unsafeRun()(cache.ec) match {
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
      options.sparkVersion
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
    sparkVersionOpt: Option[String]
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

    // FIXME Add more?
    // (see "cs complete-dependency org.apache.spark: | grep '_2\.12$'"
    // or `ls "$(cs get https://archive.apache.org/dist/spark/spark-2.4.2/spark-2.4.2-bin-hadoop2.7.tgz --archive)"/*/jars | grep '^spark-'`)
    val sparkModules = Seq(
      "core",
      "graphx",
      "hive",
      "hive-thriftserver",
      "kubernetes",
      "mesos",
      "mllib",
      "repl",
      "sql",
      "streaming",
      "yarn"
    )

    val params = ScalaParameters(scalaVersion)
    val sparkDependencies = sparkModules.map { mod =>
      dep"org.apache.spark::spark-$mod:$sparkVersion".applyParams(params).toCs
    }
    val extraDependencies = Seq(
      dep"com.github.scopt::scopt:3.7.1".applyParams(params).toCs
    )
    val dependencies = sparkDependencies ++ extraDependencies

    System.err.println(s"Fetching Spark JARs via coursier")
    val res = Fetch()
      .addDependencies(dependencies: _*)
      .runResult()(FileCache().ec)
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

    for (p <- os.walk.stream(distribPath)) {
      val rel = p.relativeTo(distribPath).asSubPath
      if (os.isDir(p))
        os.makeDir(dest / rel)
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

    val csSh = FileCache().file(Artifact(csShUrl)).run.unsafeRun()(FileCache().ec) match {
      case Left(e) => throw new Exception(e)
      case Right(f) => os.Path(f, os.pwd)
    }

    val (destBase, dropHead) = os.list(dest) match {
      case Seq(dir) if os.isDir(dir) => (dir, true)
      case _ => (dest, false)
    }
    val fetchJarsDir = destBase / "fetch-jars"
    os.makeDir.all(fetchJarsDir)
    os.copy(csSh, fetchJarsDir / "cs.sh")
    if (!Properties.isWin)
      os.perms.set(fetchJarsDir / "cs.sh", "rwxr-xr-x")

    def fetchJarContent =
      """#!/usr/bin/env bash
        |set -eu
        |
        |cd "$(dirname "${BASH_SOURCE[0]}")"
        |
        |cat fetch-jars/jar-urls | while read entry; do
        |  url="$(echo "$entry" | sed 's/->.*$//')"
        |  dest="$(echo "$entry" | sed 's/^.*->//')"
        |  echo "Getting $dest from $url" 1>&2
        |  cp "$(./fetch-jars/cs.sh get "$url")" "$dest"
        |done
        |""".stripMargin.getBytes(StandardCharsets.UTF_8)
    os.write(destBase / "fetch-jars.sh", fetchJarContent, perms = if (Properties.isWin) null else "rwxr-xr-x")

    def entriesContent =
      entries
        .toList
        .map {
          case (url, dest) =>
            val dest0 = if (dropHead) dest.segments.drop(1).mkString("/") else dest.toString
            s"$url->$dest0${System.lineSeparator()}"
        }
        .mkString
        .getBytes(StandardCharsets.UTF_8)
    os.write(fetchJarsDir / "jar-urls", entriesContent)
  }
}
