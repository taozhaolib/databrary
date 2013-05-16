package util

object maybe {
  def apply(s : String) =
    if (s.isEmpty) None else Some(s)
}
