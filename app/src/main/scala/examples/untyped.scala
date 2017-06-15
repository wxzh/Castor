package examples

import utils._

  @visitor
  trait untyped_TmVis extends varapp_TmVis {
    def tmAbs: (String, Tm) => Tm
    override def tmVar: (Int, Int) => Tm
    override def tmApp: (Tm, Tm) => Tm
  }

  trait untyped_TmMap extends untyped_TmVis {
    val onVar: (Int, Int) => OTm
    type OTm = Int => Tm
    def tmVar = onVar
    def tmAbs = (x,t) => c => TmAbs(x,visitTm(t)(c+1))
    def tmApp = (t1,t2) => c => TmApp(visitTm(t1)(c),visitTm(t2)(c))
  }

  trait untyped_Eval1 extends untyped_TmVis with varapp_Eval1 {
    override type OTm = Tm

    def tmApp = {
      case (TmAbs(x, t), t2) if isVal(t2) => termSubstTop(t2, t)
      case (t1, t2) if isVal(t1) => TmApp(t1, visitTm(t2))
      case (t1, t2) => TmApp(visitTm(t1), t2)
    }
  }

  trait untyped_IsVal extends untyped_TmVis with varapp_IsVal {
    override type OTm = Boolean

    def tmAbs = (_, _) => true
  }

