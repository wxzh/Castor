package tapl.extracted

import examples._
import util.Print._

@family
@adts(Binding)
@ops(BindingShift, PBinding)
trait Let extends VarApp {
  @adt trait Tm extends super.Tm {
    case class TmLet(l: String, t1: Tm, t2: Tm)
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmLet = x => (onvar,c) => TmLet(x.l, this(x.t1)(onvar,c), this(x.t2)(onvar,c+1))
  }

  @visit(Tm) trait PtmTerm extends super.PtmTerm {
    def tmLet = x => (_,ctx) =>
      g0("let " :: x.l :: " = " :: ptmTerm(x.t1)(false, ctx) :/: "in" :/: ptmTerm(x.t2)(false, ctx.addName(x.l)))
  }

  @default(Tm) trait PtmATerm extends super.PtmATerm

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm

  @default(Tm) trait IsVal extends super.IsVal

  @visit(Tm) trait Eval1 extends super.Eval1 {
    def tmLet = {
      case TmLet(x, v1, t2) if isVal(v1) => _ =>
        termSubstTop(v1, t2)
      case TmLet(x, t1, t2) => ctx =>
        TmLet(x,this(t1)(ctx),t2)
    }
  }
}

@family
@adts(Binding,Ty)
@ops(BindingShift,PBinding,GetTypeFromBind,PtyType,PtyAType,PtyArrowType,Subtype,TyEqv)
trait TyLet extends Let with Typed {
  @adt trait Tm extends super[Let].Tm with super[Typed].Tm

  @visit(Tm) trait Typeof extends super[Typed].Typeof {
    def tmLet = x => ctx => {
      val tyT1 = this(x.t1)(ctx)
      val ctx1 = ctx.addBinding(x.l, VarBind(tyT1))
      this(x.t2)(ctx1)
    }
  }
  @visit(Tm) trait Eval1 extends super[Let].Eval1 with super[Typed].Eval1
  @visit(Tm) trait IsVal extends super[Let].IsVal with super[Typed].IsVal
  @visit(Tm) trait PtmTerm extends super[Let].PtmTerm with super[Typed].PtmTerm
  @visit(Tm) trait PtmATerm extends super[Let].PtmATerm with super[Typed].PtmATerm
  @visit(Tm) trait PtmAppTerm extends super[Let].PtmAppTerm with super[Typed].PtmAppTerm
  @visit(Tm) trait TmMap extends super[Let].TmMap with super[Typed].TmMap
}
