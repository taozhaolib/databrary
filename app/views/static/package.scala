package views.html

package static {
  import play.api.templates._
  case class Page(template : Template0[HtmlFormat.Appendable], path : String, name : String)
}

package object static {
  final val pages = Seq(
    Page(about,        "about",        "About the Project")
  , Page(board,        "board",        "Advisory Board")
  , Page(contact,      "contact",      "Contact Us")
  , Page(contributors, "contributors", "Data Contributors")
  , Page(faq,          "faq",          "Frequently Asked Questions")
  , Page(jobs,         "jobs",         "Join Our Team")
  , Page(policies,     "policies",     "Policies")
  ).map(p => p.path -> p).toMap
}
