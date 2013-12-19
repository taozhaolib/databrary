package views.html

import play.api.libs.json.Json
import play.api.templates.Html
import site._

package static {
  import play.api.templates._
  case class Page(template : Template0[HtmlFormat.Appendable], path : String, title : String, parent : Boolean = false)
}

package object static {
  val pages = Seq(
    Page(about, "about", "About the Project", true)
  , Page(faq, "faq", "F.A.Q.", true)
  , Page(policies, "policies", "Policies", true)
  , Page(cipher, "team", "Our Team", true)
  , Page(board, "board", "Advisory Board", false)
  , Page(contributors, "contributors", "Contributors", false)
  , Page(contact, "contact", "Contact Us", true)
  , Page(jobs, "jobs", "Jobs", true)
  ).map(p => p.path -> p).toMap

  val jsonPages = Html(JsonArray.map((page : Page) => JsonObject(
      'path -> page.path,
      'title -> page.title,
      'parent -> page.parent
    ))(pages.values).toString)
}
