package examples

case class Context[Bind](l: List[(String,Bind)]) {
  def length = l.length
  def addBinding(s: String, bind: Bind): Context[Bind] =
    Context((s,bind)::l)
//  def addName(s: String) = addBinding(s, NameBind)
  def isNameBound(s: String): Boolean = l.exists {_._1 == s}
  def pickFreshName(n: String): (Context[Bind], String) =
//    if (isNameBound(n))
      pickFreshName(n + "_")
//    else
//      (Context(n,NameBind)::l, n)
  def index2name(i: Int): String = l(i)._1
  def name2index(s: String): Int = l.indexWhere(_._1 == s) match {
    case -1 => throw new Exception("identifier" + s + " is unbound")
    case i => i
  }
  def getBinding(i: Int): Bind = l(i)._2
}

