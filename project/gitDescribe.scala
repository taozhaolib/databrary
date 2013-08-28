import sbt._

object GitDescribe extends Plugin {
  val gitDescribeOptions = SettingKey[Seq[String]]("git-describe-options", "Arguments to pass to git-describe")
  val gitDescribe = TaskKey[Option[String]]("git-describe", "Determine the git version description of the current revision")

  override val buildSettings = Seq(
    gitDescribeOptions := Seq("--dirty"),
    gitDescribe <<= gitDescribeOptions map { opts =>
      Process(Seq("git", "describe", "--always") ++ opts).lines_!.headOption
    }
  )
}
