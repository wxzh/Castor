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
    case class TmAbs(x: String, ty: Ty, t: Tm)
  }

  @adt trait Ty extends super.Ty {
    case class TyArr(ty1: Ty, ty2: Ty)
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmAbs = x => (onvar, c) => TmAbs(x.x, x.ty, this(x.t)(onvar, c + 1))
  }

  @visit(Tm) trait PtmTerm extends super.PtmTerm {
    def tmAbs = x => (outer, ctx) => {
      val (ctx1, x1) = ctx.pickFreshName(x.x)
      val abs = g0("\\" :: x1 :: ":" :: ptyType(x.ty)(false, ctx) :: ".")
      val body = ptmTerm(x.t)(outer, ctx1)
      g2(abs :/: body)
    }
  }

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm

  @default(Tm) trait PtmATerm extends super.PtmATerm

  @default(Ty) trait PtyType extends super.PtyType {
    override def ty = ptyArrowType(_)
  }

  @default(Ty) trait PtyArrowType {
    type OTy = (Boolean, Context) => Document

    override def tyArr = x => (outer, ctx) =>
      g0(ptyAType(x.ty1)(false, ctx) :: " ->" :/: this (x.ty2)(outer, ctx))

    def ty = ptyAType(_)
  }

  @default(Ty) trait PtyAType extends super.PtyAType

  @default(Tm) trait Eval1 extends super.Eval1 {
    override def tmApp = {
      case TmApp(TmAbs(_, _, t1), v2) if isVal(v2) => _ =>
        termSubstTop(v2, t1)
      case TmApp(v1, t2) if isVal(v1) => ctx =>
        val t21 = this(t2)(ctx)
        TmApp(v1, t21)
      case TmApp(t1, t2) => ctx =>
        val t11 = this(t1)(ctx)
        TmApp(t11, t2)
    }
  }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmVar = x => ctx =>
      getTypeFromBind(ctx.getBinding(x.i))

    def tmAbs = x => ctx => {
      val ctx1 = ctx.addBinding(x.x, VarBind(x.ty))
      val ty1 = this (x.t)(ctx1)
      TyArr(x.ty, ty1)
    }

    def tmApp = x => ctx => {
      val tyT1 = this (x.t1)(ctx)
      val tyT2 = this (x.t2)(ctx)
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
    def tmAbs = _ => true
  }

  @visit(Ty) trait Subtype extends super.Subtype with TyEqv

  @visit(Ty) trait TyEqv extends super.TyEqv {
    def tyArr = x => {
      case TyArr(ty1,ty2) => this(ty1)(x.ty1) && this(x.ty2)(ty2)
    }
  }
}
