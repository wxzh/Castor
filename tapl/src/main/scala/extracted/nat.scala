package tapl.extracted

import examples._
import util.Document
import util.Document._
import util.Print._

@adts(Binding)
@ops(BindingShift, PBinding)
@family trait Nat extends Term {

  @adt trait Tm extends super.Tm {
    def TmZero: Tm
    def TmSucc: Tm => Tm
    def TmPred: Tm => Tm
  }

  @default(Tm) trait TmMap extends super.TmMap {
    override def tmSucc = t => (onvar,c) => TmSucc(this(t)(onvar,c))
    override def tmPred = t => (onvar,c) => TmPred(this(t)(onvar,c))
  }

  @default(Tm) trait PtmTerm extends super.PtmTerm

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm {
    override def tmPred = t => (_,ctx) =>
      "pred " :: ptmATerm(t)(false,ctx)
  }

  @default(Tm) trait PtmATerm extends super.PtmATerm {
    override def tmZero = (_,_) => "0"
    override def tmSucc = t => (_,ctx) => {
      def pf(i: Int, t: Tm): Document = t match {
        case TmZero =>
          i.toString
        case TmSucc(s) =>
          pf(i + 1, s)
        case _ =>
          "(succ " :: this(t)(false,ctx) :: ")"
      }
      pf(1, t)
    }
  }

  @default(Tm) trait IsVal extends super.IsVal {
    override def tmZero = true
  }

  def isNumericVal(t: Tm): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => isNumericVal(t1)
    case _ => false
  }

  @default(Tm) trait Eval1 extends super.Eval1 {
    override def tmSucc = t => ctx =>
      TmSucc(this(t)(ctx))
    override def tmPred = {
      case TmZero => _ => TmZero
      case TmSucc(t) if isNumericVal(t) => _ => t
      case t => ctx => TmPred(this(t)(ctx))
    }
  }
}

@family
@adts(Binding,Tm)
@ops(BindingShift, PBinding, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap)
trait TyNat extends Type with Nat {
  @adt trait Ty extends super.Ty {
    def TyNat: Ty
  }

  @default(Ty) trait PtyType extends super.PtyType

  @visit(Ty) trait PtyAType extends super.PtyAType {
    def tyNat = (_,_) => "Nat"
  }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmZero = _ => TyNat
    def tmSucc = t => ctx =>
      if (this(t)(ctx) == TyNat) {
        TyNat
      } else {
        throw new Exception("argument of Succ: is not a number: " + t)
      }
    def tmPred = t => ctx =>
      if (this(t)(ctx) == TyNat) {
        TyNat
      } else {
        throw new Exception("argument of Pred: is not a number: " + t)
      }
  }

  @visit(Ty) trait TyEqv extends super.TyEqv {
    def tyNat = { case TyNat => true }
  }

  @visit(Ty) trait Subtype extends TyEqv with super.Subtype
}
