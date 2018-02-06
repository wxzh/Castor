package tapl.untyped

import util.Print._
import util.Document._
import tapl.extracted._
import examples._

@adts(Binding)
@ops(BindingShift, PBinding)
@family trait Untyped extends VarApp {
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
