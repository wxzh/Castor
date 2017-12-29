package tapl.untyped

import util.Print._
import tapl.arith._
import util.Document
import util.Document._
import examples._


@adts(Binding)
@ops(BindingShift, PBinding)
@vicase trait VarApp extends Binding with Term {
  @adt trait Tm extends super.Tm {
    def TmVar: (Int, Int) => Tm
    def TmApp: (Tm, Tm) => Tm
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmVar = (x, n) => (onvar,c) => onvar(c, x, n)
    def tmApp = (t1, t2) => (onvar,c) => TmApp(this (t1)(onvar,c), this (t2)(onvar,c))
  }

  def termShiftAbove(d: Int, t: Tm, c: Int) =
    tmMap(t)((c, x, n) => if (x >= c) TmVar(x + d, n + d) else TmVar(x, n + d), c)

  def termShift(d: Int, t: Tm): Tm =
    termShiftAbove(d,t,0)

  // usual substitution: [j -> s]
  def termSubst(j: Int, s: Tm, t: Tm): Tm = {
    val f = (c: Int, i: Int, cl: Int) => {
      if (i == j + c) termShift(c, s) else TmVar(i, cl)
    }
    tmMap(t)(f,0)
  }

  def termSubstTop(s: Tm, t: Tm): Tm =
    termShift(-1, termSubst(0, termShift(1, s), t))

  @default(Tm) trait PtmTerm extends super.PtmTerm

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm {
    override def tmApp = (t1, t2) => (_, ctx) =>
      g2(this (t1)(false, ctx) :/: ptmATerm(t2)(false, ctx))
  }

  @default(Tm) trait PtmATerm extends super.PtmATerm {
    override def tmVar = (x, n) => (_, ctx) =>
      if (ctx.length == n) ctx.index2Name(x)
      else text("[bad index: " + x + "/" + n + " in {" + ctx.l.mkString(", ") + "}]")
  }

  @default(Tm) trait IsVal extends super.IsVal {
    override def tmVar = (_, _) => true
  }

  @default(Tm) trait Eval1 extends super.Eval1
}

@adts(Binding)
@ops(BindingShift, PBinding)
@vicase trait Untyped extends VarApp {
  @adt trait Tm extends super.Tm {
    def TmAbs: (String, Tm) => Tm
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmAbs = (x,t) => (onvar,c) => TmAbs(x, this(t)(onvar,c + 1))
  }

  @visit(Tm) trait PtmTerm extends super.PtmTerm {
    def tmAbs = (x, t) => (outer, ctx) => {
      val (ctx1, x1) = ctx.pickFreshName(x)
      val abs = g0("\\" :: x1 :: ".")
      val body = apply(t)(outer, ctx1)
      g2(abs :/: body)
    }
  }

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm

  @default(Tm) trait PtmATerm extends super.PtmATerm

  @default(Tm) trait Eval1 extends super.Eval1 {
    override def tmApp = {
      case (TmAbs(_,t1),v2) if isVal(v2) => _ =>
        termSubstTop(v2,t1)
      case (v1, t2) if isVal(v1) => ctx =>
        val t21 = this(t2)(ctx)
        TmApp(v1,t21)
      case (t1, t2) => ctx =>
        val t11 = this(t1)(ctx)
        TmApp(t11, t2)
    }
  }

  @visit(Tm) trait IsVal extends super.IsVal {
    def tmAbs = (_,_) => true
  }
}