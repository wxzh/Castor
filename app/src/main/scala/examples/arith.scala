package examples

@visitor
trait arith_TmVis extends nat_TmVis with bool_TmVis {
  def tmIsZero: Tm => Tm
  override def tmTrue: Tm
  override def tmFalse: Tm
  override def tmIf: (Tm, Tm, Tm) => Tm
  override def tmZero: Tm
  override def tmSucc: Tm => Tm
  override def tmPred: Tm => Tm
}

trait arith_Eval1 extends arith_TmVis with nat_Eval1 with bool_Eval1 {
  override type OTm = Tm
  def tmIsZero = {
    case TmZero => TmZero()
    case TmSucc(nv1) if isNumericVal(nv1) => TmFalse()
    case t => TmIsZero(visitTm(t))
  }
}

trait arith_IsNumericVal extends arith_TmVis with nat_IsNumericVal

trait arith_IsVal extends arith_TmVis with bool_IsVal with nat_IsNumericVal {
  val isNumericVal: Tm => Boolean
  override type OTm = Boolean
  def tmIsZero = _ => false
  def apply(t: Tm) = isNumericVal(t) || visitTm(t)
}

trait arith_Print extends arith_TmVis with nat_Print with bool_Print {
  override type OTm = String
  def tmIsZero = "(iszero" + visitTm(_) + ")"
}