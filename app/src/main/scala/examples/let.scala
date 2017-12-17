package examples

/*
trait let_TmVis extends varapp_TmVis {
  type Tm
  def tmLet: (String, Tm, Tm) => Tm
  override def tmApp: (Tm, Tm) => Tm
  override def tmVar: (Int, Int) => Tm
}

trait let_Eval1 extends let_TmVis with varapp_Eval1 {
  def tmLet = (x,t1,t2) =>
    if (isVal(t1)) termSubstTop(t1,t2)
    else TmLet(x,visitTm(t1),t2)
}

//trait let_Print[Tm] extends let_TmVis[Tm] with va

trait let_Typeof extends varapp_TmVis {
  def tmLet = (x,t1,t2) => {
    val tyT1 = visitTm(t1)(ctx)
    visitTm(t2)(ctx.addBinding(x,VarBind()))
  }

}
*/