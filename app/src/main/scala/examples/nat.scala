package examples

import utils._

object NoRuleApplies extends Exception

@vicase trait Term {
  @adt trait Tm

  @default(Tm) trait Eval1 {
    type OTm = Tm
    def otherwise = _ => throw NoRuleApplies
  }
}

@vicase trait Nat extends Term {
  @adt trait Tm extends super.Tm {
    def TmZero: Tm
    def TmSucc: Tm => Tm
    def TmPred: Tm => Tm
  }
  @default(Tm) trait Eval1 extends super.Eval1 {
    override def tmSucc = t => TmSucc(this(t))
    override def tmPred = {
      case TmZero => TmZero
      case TmSucc(t) if nv(t) => t
      case t => TmPred(this(t))
    }
  }
  def nv(t: Tm): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => nv(t1)
    case _ => false
  }
}
