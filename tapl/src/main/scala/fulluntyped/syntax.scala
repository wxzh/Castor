package tapl.fulluntyped

import tapl.extracted._
import tapl.untyped._
import tapl.arith.Arith
import examples._


@adts(Binding)
@ops(BindingShift, PBinding)
@family
trait FullUntyped extends Untyped with Arith with Record with Let with Str with TmAbbBinding {
  @adt trait Tm extends super[Untyped].Tm with super[Arith].Tm with super[Record].Tm with super[Let].Tm with super[Str].Tm
  @visit(Tm) trait IsVal extends super[Untyped].IsVal with super[Arith].IsVal with super[Record].IsVal with super[Let].IsVal with super[Str].IsVal
  @visit(Tm) trait TmMap extends super[Untyped].TmMap with super[Arith].TmMap with super[Record].TmMap with super[Let].TmMap with super[Str].TmMap
  @visit(Tm) trait PtmTerm extends super[Untyped].PtmTerm with super[Arith].PtmTerm with super[Record].PtmTerm with super[Let].PtmTerm with super[Str].PtmTerm
  @visit(Tm) trait PtmAppTerm extends super[Untyped].PtmAppTerm with super[Arith].PtmAppTerm with super[Record].PtmAppTerm with super[Let].PtmAppTerm with super[Str].PtmAppTerm
  @visit(Tm) trait PtmATerm extends super[Untyped].PtmATerm with super[Arith].PtmATerm with super[Record].PtmATerm with super[Let].PtmATerm with super[Str].PtmATerm
  @default(Tm) trait PtmPathTerm extends super[Record].PtmPathTerm
  @visit(Tm) trait Eval1 extends super[Untyped].Eval1 with super[Arith].Eval1 with super[Record].Eval1 with super[Let].Eval1 with super[Str].Eval1 with super[TmAbbBinding].Eval1
}

