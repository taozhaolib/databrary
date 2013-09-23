import sbt._

object GitDescribe extends Plugin {
  val gitDescribeOptions = SettingKey[Seq[String]]("git-describe-options", "Arguments to pass to git-describe")
  val gitDescribe = SettingKey[Option[String]]("git-describe", "The git version description of the current revision")

  def get(opts : String*) =
    Process(Seq("git", "describe", "--always") ++ opts).lines_!.headOption

  override val buildSettings = Seq(
    gitDescribeOptions := Seq("--dirty"),
    gitDescribe <<= gitDescribeOptions.apply(get(_ : _*))
  )
}
