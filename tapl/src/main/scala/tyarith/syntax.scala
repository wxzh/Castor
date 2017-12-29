package tapl.tyarith

import tapl.arith._
import util.Document
import util.Document._
import util.Print._
import examples._

@adts(Binding,Tm)
@ops(BindingShift, PBinding, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap)
@vicase trait Type extends Term {
  @adt trait Ty

  @default(Ty) trait PtyType {
    type OTy = (Boolean,Context) => Document
    def otherwise = ptyAType(_)
  }

  @default(Ty) trait PtyAType {
    type OTy = (Boolean,Context) => Document
    def otherwise = ty => "(" :: ptyType(ty)(_,_) :: ")"
  }

  def ptyTy(ty: Ty, ctx: Context) = ptyType(ty)(true,ctx)

  @visit(Tm) trait Typeof {
    type OTm = Context => Ty
  }

//  trait TyMap extends TyDefault {_: TyV =>
//    type T = Int => Ty
//    val onVar: (Int,Int,Int) => Ty
//    def otherwise = ty => _ => ty
//  }

  @visit(Ty) trait TyEqv {
    type OTy = Ty => Boolean
  }

  @visit(Ty) trait Subtype extends TyEqv
}

@adts(Binding,Tm)
@ops(BindingShift, PBinding, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap)
@vicase trait TyNat extends Type with Nat {
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

  @visit(Ty) trait Subtype extends super.Subtype with TyEqv
}

@adts(Binding,Tm)
@ops(BindingShift, PBinding, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap)
@vicase
trait TyBool extends Type with Bool {
  @adt trait Ty extends super.Ty {
    def TyBool: Ty
  }

  @default(Ty) trait PtyType extends super.PtyType

  @visit(Ty) trait PtyAType extends super.PtyAType {
    def tyBool = (_,_) => "Bool"
  }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmTrue = _ => TyBool
    def tmFalse = _ => TyBool
    def tmIf = (t1,t2,t3) => ctx =>
      if (this(t1)(ctx) == TyBool) {
        val ty2 = this(t2)(ctx)
        if (ty2 == this(t3)(ctx)) {
          ty2
        } else {
          throw new Exception("arms of conditional " + TmIf(t1, t2, t3) + " have different types")
        }
      } else {
        throw new Exception("guard of conditional " + TmIf(t1,t2,t3) + " is not a boolean")
      }
  }

  @visit(Ty) trait TyEqv extends super.TyEqv {
    def tyBool = { case TyBool => true }
  }

  @visit(Ty) trait Subtype extends super.Subtype with TyEqv
}

@adts(Binding,Tm)
@ops(BindingShift, PBinding, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap)
@vicase
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