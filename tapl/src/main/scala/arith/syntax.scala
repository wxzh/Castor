package tapl.arith

import examples._
import tapl.extracted._
import util.Document._

@adts(Binding)
@ops(BindingShift, PBinding)
@family trait Arith extends Nat with Bool {
  @adt trait Tm extends super[Nat].Tm with super[Bool].Tm {
    def TmIsZero: Tm => Tm
  }

  @visit(Tm) trait TmMap extends super[Nat].TmMap with super[Bool].TmMap {
    def tmIsZero = t => (onvar,c) => TmIsZero(this(t)(onvar,c))
  }

  @default(Tm) trait PtmTerm extends super[Nat].TmDefault with super[Bool].PtmTerm

  @visit(Tm) trait PtmAppTerm extends super[Nat].PtmAppTerm with super[Bool].TmDefault {
    override def tmIsZero = t => (_,ctx) =>
      "iszero " :: ptmATerm(t)(false,ctx)
  }

  @default(Tm) trait PtmATerm extends super[Nat].PtmATerm with super[Bool].PtmATerm

  @visit(Tm) trait Eval1 extends super[Nat].Eval1 with super[Bool].Eval1 {
    def tmIsZero = {
      case TmZero => _ => TmTrue
      case TmSucc(nv) if isNumericVal(nv) => _ => TmFalse
      case t => ctx => TmIsZero(this(t)(ctx))
    }
  }

  @default(Tm) trait IsVal extends super[Nat].IsVal with super[Bool].IsVal {
    override def apply(t: Tm) = isNumericVal(t) || t.accept(this)
  }
}
