package object site {
  implicit def siteDB(implicit site : Site) : Site.DB = site.db

  /** Group adjacent elements with identical keys into nested lists, such that the concatenation of the resulting _2 elements is the original list.
    * This is more like Haskell's group than scala's [[Seq#groupBy]]: only adjacent elements are grouped.
    * @param l the list to group
    * @param f the key-generating function to group by
    */
  def groupBy[A,K](l : Seq[A], f : A => K) : Seq[(K,Seq[A])] = {
    val r = l.genericBuilder[(K,Seq[A])]
    @scala.annotation.tailrec def next(l : Seq[A]) : Unit = if (l.nonEmpty) {
      val k = f(l.head)
      val (p, s) = l.span(f(_) == k)
      r += k -> p
      next(s)
    }
    next(l)
    r.result
  }
}
