import caseapp._

// format: off
final case class ConvertOptions(
  dest: String,
  @HelpMessage("Whether the Spark distribution pointed by the passed URL might change in the future")
    changing: Boolean = false,
  @HelpMessage("Erase destination path if it already exists")
    force: Boolean = false,
  @HelpMessage("Create an archive rather than a Spark distribution directory")
    archive: Boolean = false,
  @HelpMessage("Force Spark version")
  @ExtraName("spark")
    sparkVersion: Option[String] = None,
  @HelpMessage("Force Scala version")
  @ExtraName("scala")
    scalaVersion: Option[String] = None,
  @HelpMessage("Re-create the YARN shuffle service JAR right away, and compare its entry names with those of the one the Spark distribution ships")
    checkYarnShuffleJar: Boolean = false,
  @HelpMessage("Command running the coursier CLI, used by --check-yarn-shuffle-jar (defaults to the cs.sh script the generated distribution uses). Pass several times for a command made of several arguments.")
  @ExtraName("cs")
    csCommand: List[String] = Nil
)
// format: on

object ConvertOptions {
  implicit lazy val parser: Parser[ConvertOptions] = Parser.derive
  implicit lazy val help: Help[ConvertOptions]     = Help.derive
}
