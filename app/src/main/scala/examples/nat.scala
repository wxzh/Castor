package examples

import utils._

  @visitor
  trait nat_TmVis {
    type Tm
    def tmZero: Tm

    def tmSucc: Tm => Tm

    def tmPred: Tm => Tm
  }

  trait nat_Eval1 extends nat_TmVis {
    type OTm = Tm
    implicit val isNumericVal: Tm => Boolean

    def tmZero = throw NoRuleApplies

    def tmSucc = t =>
      TmSucc(visitTm(t))

    def tmPred = {
      case TmZero => TmZero()
      case TmSucc(nv1) if isNumericVal(nv1) =>
        nv1
      case t =>
        val t1 = visitTm(t)
        TmPred(t1)
    }
  }

  trait nat_IsNumericVal extends nat_TmVis {
    type OTm = Boolean

    def tmZero = true

    def tmSucc = visitTm(_)

    def tmPred = _ => false
  }

  @visitor trait nat_TyVis {
    type Ty
    def tyNat: Ty
  }

  trait nat_Typeof[Ty] extends nat_TmVis {
    type OTm = Ty
    val ty: nat_TyVis.Fact[Ty]
    val tyEqv: (Ty, Ty) => Boolean

    def tmZero = ty.TyNat()

    def tmSucc = t =>
      if (tyEqv(visitTm(t), ty.TyNat()))
        ty.TyNat()
      else
        throw new Exception("argument of succ is not a number")

    def tmPred = tmSucc
  }

  trait nat_TyEqv extends nat_TyVis {//with ITyEqv[Ty] {
    type OTy = Ty => Boolean

    def tyNat = {
      case TyNat => true
    }
  }

trait nat_Print extends nat_TmVis {
  type OTm = String

  def tmZero = "0"
  def tmSucc = printConsecutiveSuccs(1)

  private def printConsecutiveSuccs(i: Int): Tm => String = {
    case TmSucc(t) => printConsecutiveSuccs(i+1)(t)
    case TmZero => i.toString
    case t  => "(succ" + visitTm(t) + ")"
  }
}

trait nat_PrintTy extends nat_TyVis {
  type OTy = String
  def tyNat = "Nat"
}