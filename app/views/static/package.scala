package views.html

import play.api.libs.json._
import play.api.templates.Html

package static {
  import play.api.templates._
  case class Page(template : Template0[HtmlFormat.Appendable], path : String, title : String, parent : Boolean = false)
}

package object static {
  final val pages = Seq(
    Page(about, "about", "About the Project", true)
    , Page(faq, "faq", "F.A.Q.", true)
    , Page(policies, "policies", "Policies", true)
    , Page(cipher, "team", "Our Team", true)
    , Page(board, "board", "Advisory Board", false)
    , Page(contributors, "contributors", "Contributors", false)
    , Page(contact, "contact", "Contact Us", true)
    , Page(jobs, "jobs", "Jobs", true)
  ).map(p => p.path -> p)

  def mapPages = pages.toMap

  def jsonPages = Html(Json.stringify(Json.toJson(pages.map {
    case (path, page) =>
      Json.toJson(Map(
        "path" -> page.path.toString,
        "title" -> page.title.toString,
        "parent" -> page.parent.toString
      ))
  })))
}
