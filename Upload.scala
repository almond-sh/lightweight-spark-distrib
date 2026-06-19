//> using scala "2.13"
//> using lib "io.github.alexarchambault.mill::mill-native-image-upload:0.1.21"
//> using lib "com.lihaoyi::os-lib:0.8.1"

object Upload {
  private def create(sourceUrl: String, dest: os.Path): Unit = {
    val extraArgs = if (System.getenv("CI") == null) Nil else Seq("--server=false")
    os.proc("scala-cli", "run", "src", extraArgs, "--", "--force", "--dest", dest, "--archive", sourceUrl)
      .call(stdin = os.Inherit, stdout = os.Inherit)
  }
  case class Versions(sparkVersion: String, hadoopVersion: String)
  private def versions = Seq(
    Versions("3.0.3", "2.7"),
    Versions("2.4.2", "2.7")
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
      val url = s"https://archive.apache.org/dist/spark/spark-$sparkVer/spark-$sparkVer-bin-hadoop$hadoopVer.tgz"
      val name = s"spark-$sparkVer-bin-hadoop$hadoopVer.tgz"
      val dest = os.temp(prefix = name, suffix = ".tgz")
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
