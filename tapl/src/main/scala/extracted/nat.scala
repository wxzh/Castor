package tapl.extracted

import examples._
import util.Document
import util.Document._
import util.Print._

@adts(Binding)
@ops(BindingShift, PBinding)
@family trait Nat extends Term {

  @adt trait Tm extends super.Tm {
    case object TmZero
    case class TmSucc(t: Tm)
    case class TmPred(t: Tm)
  }

  @default(Tm) trait TmMap extends super.TmMap {
    override def tmSucc = x => (onvar,c) => TmSucc(this(x.t)(onvar,c))
    override def tmPred = x => (onvar,c) => TmPred(this(x.t)(onvar,c))
  }

  @default(Tm) trait PtmTerm extends super.PtmTerm

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm {
    override def tmPred = x => (_,ctx) =>
      "pred " :: ptmATerm(x.t)(false,ctx)
  }

  @default(Tm) trait PtmATerm extends super.PtmATerm {
    override def tmZero = (_,_) => "0"
    override def tmSucc = x => (_,ctx) => {
      def pf(i: Int, t: Tm): Document = t match {
        case TmZero =>
          i.toString
        case TmSucc(s) =>
          pf(i + 1, s)
        case _ =>
          "(succ " :: this(t)(false,ctx) :: ")"
      }
      pf(1, x.t)
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
    override def tmSucc = x => ctx =>
      TmSucc(this(x.t)(ctx))
    override def tmPred = {
      case TmPred(TmZero) => _ => TmZero
      case TmPred(TmSucc(t)) if isNumericVal(t) => _ => t
      case TmPred(t) => ctx => TmPred(this(t)(ctx))
    }
  }
}

@family
@adts(Binding,Tm)
@ops(BindingShift, PBinding, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap)
trait TyNat extends Type with Nat {
  @adt trait Ty extends super.Ty {
    case object TyNat
  }

  @default(Ty) trait PtyType extends super.PtyType

  @visit(Ty) trait PtyAType extends super.PtyAType {
    def tyNat = (_,_) => "Nat"
  }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmZero = _ => TyNat
    def tmSucc = x => ctx =>
      if (this(x.t)(ctx) == TyNat) {
        TyNat
      } else {
        throw new Exception("argument of Succ: is not a number: " + x.t)
      }
    def tmPred = x => ctx =>
      if (this(x.t)(ctx) == TyNat) {
        TyNat
      } else {
        throw new Exception("argument of Pred: is not a number: " + x.t)
      }
  }

  @visit(Ty) trait TyEqv extends super.TyEqv {
    def tyNat = { case TyNat => true }
  }

  @visit(Ty) trait Subtype extends TyEqv with super.Subtype
}
