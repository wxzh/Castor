package tapl.tyarith

import tapl.arith._
import tapl.extracted._
import examples._


@family
@adts(Binding,Tm)
@ops(BindingShift, PBinding, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap)
trait TyArith extends TyNat with TyBool with Arith {
  @adt trait Ty extends super[TyNat].Ty with super[TyBool].Ty

  @visit(Tm) trait Typeof extends super[TyNat].Typeof with super[TyBool].Typeof {
    def tmIsZero = t => ctx =>
      if (this(t)(ctx) == TyNat) {
        TyBool
      } else {
        throw new Exception("argument of IsZero: is not a number: " + TmIsZero(t))
      }
  }
  @visit(Ty) trait PtyType extends super[TyNat].PtyType with super[TyBool].PtyType
  @visit(Ty) trait PtyAType extends super[TyNat].PtyAType with super[TyBool].PtyAType
  @visit(Ty) trait TyEqv extends super[TyNat].TyEqv with super[TyBool].TyEqv
  @visit(Ty) trait Subtype extends super[TyNat].Subtype with super[TyBool].Subtype
}
