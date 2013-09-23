import sbt._
import Keys._
import java.io.File

object GHPagesDoc extends Plugin {
  val ghPagesDoc = TaskKey[Unit]("gh-pages-doc", "Update the api documentation on a gh-pages branch")

  override val projectSettings = Seq(
    ghPagesDoc <<= (doc in Compile) map { api =>
      Process(Seq(new File("project", "git-commit-path").getPath, "-b", "gh-pages", "-m", "Update API docs" + GitDescribe.get("--dirty= with local changes").map(" from " + _).getOrElse(""), api.getPath + ":api")) !
    }
  )
}
