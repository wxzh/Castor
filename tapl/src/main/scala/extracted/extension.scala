package tapl.extracted

import examples._
import tapl.arith._
import tapl.tyarith._

@family
@adts(Binding)
@ops(BindingShift, PBinding, PBindingTy, GetTypeFromBind, CheckBinding)
trait Extension extends TyArith with TyRcd with TyStr with TyLet with TyVarBinding {
  @adt trait Tm extends super[TyArith].Tm with super[TyRcd].Tm with super[TyLet].Tm with super[TyStr].Tm
  @visit(Tm) trait Eval1 extends super[TyArith].Eval1 with super[TyRcd].Eval1 with super[TyLet].Eval1 with super[TyStr].Eval1
  @visit(Tm) trait IsVal extends super[TyArith].IsVal with super[TyRcd].IsVal with super[TyLet].IsVal with super[TyStr].IsVal
  @visit(Tm) trait PtmTerm extends super[TyArith].PtmTerm with super[TyRcd].PtmTerm with super[TyLet].PtmTerm with super[TyStr].PtmTerm
  @visit(Tm) trait PtmATerm extends super[TyArith].PtmATerm with super[TyRcd].PtmATerm with super[TyLet].PtmATerm with super[TyStr].PtmATerm
  @visit(Tm) trait PtmAppTerm extends super[TyArith].PtmAppTerm with super[TyRcd].PtmAppTerm with super[TyLet].PtmAppTerm with super[TyStr].PtmAppTerm
  @default(Tm) trait PtmPathTerm extends super[TyRcd].PtmPathTerm
  @visit(Tm) trait TmMap extends super[TyArith].TmMap with super[TyRcd].TmMap with super[TyLet].TmMap with super[TyStr].TmMap

  @adt trait Ty extends super[TyArith].Ty with super[TyRcd].Ty with super[TyStr].Ty with super[TyLet].Ty
  @visit(Tm) trait Typeof extends super[TyArith].Typeof with super[TyRcd].Typeof with super[TyStr].Typeof with super[TyLet].Typeof
  @visit(Ty) trait PtyType extends super[TyArith].PtyType with super[TyRcd].PtyType with super[TyStr].PtyType with super[TyLet].PtyType
  @visit(Ty) trait PtyAType extends super[TyArith].PtyAType with super[TyRcd].PtyAType with super[TyStr].PtyAType with super[TyLet].PtyAType
  @default(Ty) trait PtyArrowType extends super[TyLet].PtyArrowType
  @visit(Ty) trait Subtype extends super[TyArith].Subtype with super[TyRcd].Subtype with super[TyStr].Subtype with super[TyLet].Subtype
  @visit(Ty) trait TyEqv extends super[TyArith].TyEqv with super[TyRcd].TyEqv with super[TyStr].TyEqv with super[TyLet].Subtype
}
