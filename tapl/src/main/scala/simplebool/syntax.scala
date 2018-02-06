package tapl.simplebool

import tapl.extracted._
import examples._

@family
@adts(Binding)
@ops(BindingShift, PBinding, GetTypeFromBind)
trait SimpleBool extends Typed with TyBool {
  @adt trait Tm extends super[Typed].Tm with super[TyBool].Tm
  @adt trait Ty extends super[Typed].Ty with super[TyBool].Ty

  @visit(Tm) trait Eval1 extends super[Typed].Eval1 with super[TyBool].Eval1
  @visit(Tm) trait IsVal extends super[Typed].IsVal with super[TyBool].IsVal
  @visit(Tm) trait PtmTerm extends super[Typed].PtmTerm with super[TyBool].PtmTerm
  @visit(Tm) trait PtmAppTerm extends super[Typed].PtmAppTerm with super[TyBool].PtmAppTerm
  @visit(Tm) trait PtmATerm extends super[Typed].PtmATerm with super[TyBool].PtmATerm
  @visit(Tm) trait TmMap extends super[Typed].TmMap with super[TyBool].TmMap
  @visit(Tm) trait Typeof extends super[Typed].Typeof with super[TyBool].Typeof

  @visit(Ty) trait PtyType extends super[Typed].PtyType with super[TyBool].PtyType
  @visit(Ty) trait PtyArrowType extends super[Typed].PtyArrowType {
    def tyBool = otherwise(TyBool)
  }
  @visit(Ty) trait PtyAType extends super[Typed].PtyAType with super[TyBool].PtyAType
  @visit(Ty) trait TyEqv extends super[Typed].TyEqv with super[TyBool].TyEqv
  @visit(Ty) trait Subtype extends super[Typed].Subtype with super[TyBool].Subtype
}
