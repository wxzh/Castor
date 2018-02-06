package tapl.rcdsubbot

import examples._
import tapl.extracted._
import tapl.bot._


@family
@adts(Binding)
@ops(PBinding,BindingShift,GetTypeFromBind)
trait RcdSubBot extends Bot with TyRcd {
  @adt trait Tm extends super[Bot].Tm with super[TyRcd].Tm
  @adt trait Ty extends super[Bot].Ty with super[TyRcd].Ty

  @visit(Tm) trait Typeof extends super[Bot].Typeof with super[TyRcd].Typeof {
    override def tmProj = (t,l) => ctx =>
      this(t)(ctx) match {
        case TyBot => TyBot
        case _ => super.tmProj(t,l)(ctx)
      }
  }

  @visit(Tm) trait TmMap extends super[Bot].TmMap with super[TyRcd].TmMap
  @visit(Tm) trait IsVal extends super[Bot].IsVal with super[TyRcd].IsVal
  @visit(Tm) trait Eval1 extends super[Bot].Eval1 with super[TyRcd].Eval1
  @visit(Tm) trait PtmTerm extends super[Bot].PtmTerm with super[TyRcd].PtmTerm
  @visit(Tm) trait PtmAppTerm extends super[Bot].PtmAppTerm with super[TyRcd].PtmAppTerm
  @visit(Tm) trait PtmATerm extends super[Bot].PtmATerm with super[TyRcd].PtmATerm
  @default(Tm) trait PtmPathTerm extends super[TyRcd].PtmPathTerm


  @visit(Ty) trait PtyType extends super[Bot].PtyType with super[TyRcd].PtyType
  @default(Ty) trait PtyArrowType extends super[Bot].PtyArrowType
  @visit(Ty) trait PtyAType extends super[Bot].PtyAType with super[TyRcd].PtyAType
  @visit(Ty) trait Subtype extends super[Bot].Subtype with super[TyRcd].Subtype
  @visit(Ty) trait TyEqv extends super[Bot].TyEqv with super[TyRcd].TyEqv
}
