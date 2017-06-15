package examples

import utils._

@visitor
trait varapp_TmVis {
  type Tm
  def tmVar: (Int, Int) => Tm
  def tmApp: (Tm, Tm) => Tm
}


trait varapp_Eval1 extends varapp_TmVis {
  type OTm = Tm

  val termSubstTop: (Tm, Tm) => Tm
  val isVal: Tm => Boolean

  def tmVar = throw NoRuleApplies
  def tmAbs = throw NoRuleApplies
}

trait varapp_IsVal extends varapp_TmVis {
  type OTm = Boolean

  def tmVar = (_, _) => false

  def tmApp = (_, _) => false
}

//trait varapp_Print[Tm] extends varapp_TmVis[Tm] {
//  type OTm = String
//  def tmVar = (x,n) =>
//    if (ctx.length == n)
//      ctx.index2Name(x)
//    else
//      s"[bad index: $x/$n in ${ctx.printBind(...)}]"
//}
