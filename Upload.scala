//> using scala "2.13"
//> using lib "io.github.alexarchambault.mill::mill-native-image-upload:0.1.21"
//> using lib "com.lihaoyi::os-lib:0.8.1"

object Upload {
  private def create(sourceUrl: String, dest: os.Path): Unit = {
    val extraArgs = if (System.getenv("CI") == null) Nil else Seq("--server=false")
    os.proc(
      "scala-cli", "run", "src", extraArgs, "--",
      "--force", "--dest", dest, "--archive",
      // re-creating the YARN shuffle service JAR is the one thing fetch-jars.sh does that isn't a
      // plain download, so check right away that it comes out with the entries Spark ships
      "--check-yarn-shuffle-jar", "--cs", "cs",
      sourceUrl
    ).call(stdin = os.Inherit, stdout = os.Inherit)
  }
  case class Versions(sparkVersion: String, hadoopVersion: String)
  private def versions = Seq(
    Versions("4.2.0", "3"),
    Versions("4.1.2", "3"),
    Versions("4.0.3", "3"),
    Versions("3.5.8", "3"),
    Versions("3.4.4", "3"),
    Versions("3.3.4", "3"),
    Versions("3.3.4", "2"),
    Versions("3.2.4", "3.2"),
    Versions("3.2.4", "2.7"),
    Versions("3.1.3", "3.2"),
    Versions("3.1.3", "2.7"),
    Versions("3.0.3", "3.2"),
    Versions("3.0.3", "2.7"),
    Versions("2.4.8", "2.7"),
    Versions("2.4.2", "2.7")
  )
  private val isOnDlcdn = Set(
    "4.2.0",
    "4.1.2",
    "4.0.3",
    "3.5.8"
  )
  def main(args: Array[String]): Unit = {
    val tag = os.proc("git", "tag", "--points-at", "HEAD").call().out.trim()
    val dummy = tag.isEmpty
    if (dummy)
      System.err.println("Not on a git tag, running in dummy mode")
    val token = Option(System.getenv("UPLOAD_GH_TOKEN")).getOrElse {
      if (dummy) ""
      else sys.error("UPLOAD_GH_TOKEN not set")
    }
    val files = versions.map { ver =>
      val sparkVer = ver.sparkVersion
      val hadoopVer = ver.hadoopVersion
      val url =
        if (isOnDlcdn(sparkVer))
          s"https://dlcdn.apache.org/spark/spark-$sparkVer/spark-$sparkVer-bin-hadoop$hadoopVer.tgz"
        else
          s"https://archive.apache.org/dist/spark/spark-$sparkVer/spark-$sparkVer-bin-hadoop$hadoopVer.tgz"
      val name = s"spark-$sparkVer-bin-hadoop$hadoopVer.tgz"
      val dest =
        if (System.getenv("CI") == null) os.pwd / "tmp" / name
        else os.temp(prefix = name.stripSuffix(".tgz"), suffix = ".tgz")
      create(url, dest)
      dest -> s"$name.tgz"
    }
    if (!dummy)
      io.github.alexarchambault.millnativeimage.upload.Upload.upload(
        ghOrg = "almond-sh",
        ghProj = "lightweight-spark-distrib",
        ghToken = token,
        tag = tag,
        dryRun = false,
        overwrite = true
      )(files: _*)
  }
}
