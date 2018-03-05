package tapl.fullsimple

import examples._
import tapl.extracted._

@family
@adts(Binding)
@ops(PBinding,PBindingTy,BindingShift,GetTypeFromBind,CheckBinding)
trait FullSimple extends MoreExt with Variant {
  @adt trait Tm extends super[MoreExt].Tm with super[Variant].Tm
  @adt trait Ty extends super[MoreExt].Ty with super[Variant].Ty
  @visit(Tm) trait Eval1 extends super[MoreExt].Eval1 with super[Variant].Eval1
  @visit(Tm) trait Typeof extends super[MoreExt].Typeof with super[Variant].Typeof
  @visit(Tm) trait IsVal extends super[MoreExt].IsVal with super[Variant].IsVal
  @visit(Tm) trait PtmTerm extends super[MoreExt].PtmTerm with super[Variant].PtmTerm
  @visit(Tm) trait PtmAppTerm extends super[MoreExt].PtmAppTerm with super[Variant].PtmAppTerm
  @visit(Tm) trait PtmATerm extends super[MoreExt].PtmATerm with super[Variant].PtmATerm
  @default(Tm) trait PtmAscribeTerm extends super[MoreExt].PtmAscribeTerm
  @default(Tm) trait PtmPathTerm extends super[MoreExt].PtmPathTerm
  @visit(Tm) trait TmMap extends super[MoreExt].TmMap with super[Variant].TmMap
  @visit(Ty) trait PtyType extends super[MoreExt].PtyType with super[Variant].PtyType
  @visit(Ty) trait PtyAType extends super[MoreExt].PtyAType with super[Variant].PtyAType
  @visit(Ty) trait PtyArrowType extends super[MoreExt].PtyArrowType with super[Variant].PtyArrowType
  @visit(Ty) trait Subtype extends super[MoreExt].Subtype with super[Variant].Subtype
  @visit(Ty) trait TyEqv extends super[MoreExt].TyEqv with super[Variant].TyEqv
}
