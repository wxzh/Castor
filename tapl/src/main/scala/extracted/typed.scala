package tapl.extracted

import examples._
import util.Document
import util.Document._
import util.Print._

@adts(Binding)
@ops(BindingShift, PBinding, GetTypeFromBind)
@family
trait Typed extends VarBinding with VarApp {
  @adt trait Tm extends super.Tm {
    def TmAbs: (String, Ty, Tm) => Tm
  }

  @adt trait Ty extends super.Ty {
    def TyArr: (Ty, Ty) => Ty
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmAbs = (x, ty, t) => (onvar, c) => TmAbs(x, ty, this(t)(onvar, c + 1))
  }

  @visit(Tm) trait PtmTerm extends super.PtmTerm {
    def tmAbs = (x, ty, t) => (outer, ctx) => {
      val (ctx1, x1) = ctx.pickFreshName(x)
      val abs = g0("\\" :: x1 :: ":" :: ptyType(ty)(false, ctx) :: ".")
      val body = ptmTerm(t)(outer, ctx1)
      g2(abs :/: body)
    }
  }

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm

  @default(Tm) trait PtmATerm extends super.PtmATerm

  @default(Ty) trait PtyType extends super.PtyType {
    override def otherwise = ptyArrowType(_)
  }

  @default(Ty) trait PtyArrowType {
    type OTy = (Boolean, Context) => Document

    override def tyArr = (ty1, ty2) => (outer, ctx) =>
      g0(ptyAType(ty1)(false, ctx) :: " ->" :/: this (ty2)(outer, ctx))

    def otherwise = ptyAType(_)
  }

  @default(Ty) trait PtyAType extends super.PtyAType

  @default(Tm) trait Eval1 extends super.Eval1 {
    override def tmApp = {
      case (TmAbs(_, _, t1), v2) if isVal(v2) => _ =>
        termSubstTop(v2, t1)
      case (v1, t2) if isVal(v1) => ctx =>
        val t21 = this(t2)(ctx)
        TmApp(v1, t21)
      case (t1, t2) => ctx =>
        val t11 = this(t1)(ctx)
        TmApp(t11, t2)
    }
  }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmVar = (x, _) => ctx =>
      getTypeFromBind(ctx.getBinding(x))

    def tmAbs = (x, ty, t) => ctx => {
      val ctx1 = ctx.addBinding(x, VarBind(ty))
      val ty1 = this (t)(ctx1)
      TyArr(ty, ty1)
    }

    def tmApp = (t1, t2) => ctx => {
      val tyT1 = this (t1)(ctx)
      val tyT2 = this (t2)(ctx)
      tyT1 match {
        case TyArr(tyT11, tyT12) =>
          if (tyT2 == tyT11)
            tyT12
          else
            throw new Exception(s"parameter mismatch: $tyT2 != $tyT11")
        case _ =>
          throw new Exception(s"$tyT1: arrow type expected")
      }
    }
  }

  @visit(Tm) trait IsVal extends super.IsVal {
    def tmAbs = (_, _, _) => true
  }

  @visit(Ty) trait Subtype extends super.Subtype with TyEqv

  @visit(Ty) trait TyEqv extends super.TyEqv {
    def tyArr = (ty11,ty12) => {
      case TyArr(ty21,ty22) => this(ty21)(ty11) && this(ty12)(ty22)
    }
  }
}
