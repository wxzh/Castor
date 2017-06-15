package paper

import examples.visitor

@visitor trait UntypedTmVis {
  type Tm
  def tmVar: String => Tm
  def tmAbs: (String, Tm) => Tm
  def tmApp: (Tm, Tm) => Tm
}
trait UntypedIsVal extends UntypedTmVis {
  type OTm = Boolean
  def tmAbs = (_, _) => true
}
trait UntypedEval1 extends UntypedTmVis {
  type OTm = Tm
  val isVal: Tm => Boolean
  val substVar: (String, Tm, Tm) => Tm
  def tmApp = {
    case (TmAbs(x, t12), v2) if isVal(v2) =>
      substVar(x, v2, t12)
    case (v1, t2) if isVal(v1) =>
      TmApp(v1, visitTm(t2))
    case (t1, t2) =>
      TmApp(visitTm(t1), t2)
  }
}

@visitor trait BoolTmVis {
  type Tm
  def tmTrue: Tm
  def tmFalse: Tm
  def tmIf: (Tm, Tm, Tm) => Tm
}
trait BoolIsVal extends BoolTmVis {
  type OTm = Boolean
  def tmTrue = true
  def tmFalse = true
}
trait BoolEval1 extends BoolTmVis {
  type OTm = Tm
  val isVal: Tm => Boolean
  def tmIf = {
    case (TmTrue, t2, _) => t2
    case (TmFalse, _, t3) => t3
    case (t1, t2, t3) => TmIf(visitTm(t1), t2, t3)
  }
}

@visitor trait UntypedBoolTmVis extends UntypedTmVis with BoolTmVis {
  override type Tm
  override def tmAbs: (String, Tm) => Tm
  override def tmApp: (Tm, Tm) => Tm
  override def tmFalse: Tm
  override def tmIf: (Tm, Tm, Tm) => Tm
  override def tmTrue: Tm
  override def tmVar: String => Tm
}

trait UntypedBoolIsVal extends UntypedBoolTmVis with UntypedIsVal with BoolIsVal {
  override type OTm = Boolean
}
trait UntypedBoolEval1 extends UntypedBoolTmVis with UntypedEval1 with BoolEval1 {
  override type OTm = Tm
}

object Test {
  import UntypedBoolTmVis.Fact._
  val e = TmIf(TmFalse(), TmVar("x"), TmVar("z"))
}
