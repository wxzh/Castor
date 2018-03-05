package tapl.extracted

import examples._
import util.Print._
import util.Document._
import util.Document

@family
@adts(Binding)
@ops(BindingShift,PBinding,PBindingTy,GetTypeFromBind,CheckBinding)
trait MoreExt extends Extension {
  @adt trait Tm extends super.Tm {
    def TmFix: Tm => Tm
    def TmUnit: Tm
    def TmAscribe: (Tm,Ty) => Tm
    def TmInert: Ty => Tm
  }

  @adt trait Ty extends super.Ty {
    def TyUnit: Ty
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmFix = t => (onvar,c) => TmFix(this(t)(onvar,c))
    def tmAscribe = (t,ty) => (onvar,c) => TmAscribe(this(t)(onvar,c),ty)
    def tmUnit = (_,_) => TmUnit
    def tmInert = ty => (_,_) => TmInert(ty)
  }

  @default(Tm) trait PtmTerm extends super.PtmTerm {
    override def tmFix = t => (_,ctx) =>
      g2("fix " :: this(t)(false,ctx))
    override def tmAbs = super.tmAbs
    override def tmLet = super.tmLet
  }

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm

  @default(Tm) trait PtmATerm extends super.PtmATerm {
    override def tmUnit = (_,_) => "unit"
    override def tmInert = tyT => (_,ctx) =>
      "inert[" :: ptyType(tyT)(false, ctx) :: "]"
    override def tmString = super.tmString
  }

  @default(Tm) trait PtmPathTerm extends super.PtmPathTerm {
    override def otherwise = ptmAscribeTerm(_)
  }

  @default(Tm) trait PtmAscribeTerm {
    type OTm = (Boolean,Context) => Document
    def otherwise = ptmATerm(_)
    override def tmAscribe = (t, ty) => (outer,ctx) =>
      g0(ptmAppTerm(t)(false,ctx) :/: "as " :: ptyType(ty)(false,ctx))
  }

  @visit(Tm) trait Eval1 extends super.Eval1 {
    def tmFix = t => ctx =>
      if (isVal(t)) {
        t match {
          case TmAbs(_, _, t12) =>
            termSubstTop(TmFix(t), t12)
          case _ => throw NoRuleApplies()
        }
      }
      else
        TmFix(this (t)(ctx))
    def tmAscribe = (t,ty) => ctx =>
      if (isVal(t)) t
      else TmAscribe(this(t)(ctx),ty)
    def tmUnit = otherwise(TmUnit)
    def tmInert = ty => otherwise(TmInert(ty))

  }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmFix = t => ctx => {
      this(t)(ctx) match {
        case TyArr(ty1,ty2) =>
          if (ty1 == ty2)
            ty2
          else {
            throw new Exception(s"result of body not compatible with domain $t: $ty2 != $ty1")
          }
        case _ =>
          throw new Exception("error type expected in " + t)
      }
    }
    def tmUnit = _ => TyUnit
    def tmInert = ty => _ => ty
    def tmAscribe = (t,ty) => ctx =>
      if (subtype(this(t)(ctx))(ty))
        ty
      else
        throw new Exception("body of as-term doesn't have the expected type in " + t)

  }

  @default(Ty) trait PtyType extends super.PtyType
  @visit(Ty) trait PtyAType extends super.PtyAType {
    override def tyUnit = (_,_) => "Unit"
  }
  @default(Ty) trait PtyArrowType extends super.PtyArrowType
  @visit(Ty) trait TyEqv extends super.TyEqv {
    def tyUnit = {
      case TyUnit => true
    }
  }
  @visit(Ty) trait Subtype extends super.Subtype {
    def tyUnit = {
      case TyUnit => true
    }
  }

  @visit(Tm) trait IsVal extends super.IsVal {
    def tmUnit = true
    def tmAscribe = (_,_) => false
    def tmFix = _ => false
    def tmInert = _ => false
  }

  def evalBinding(ctx: Context, bind: Binding): Binding = bind match {
    case TmAbbBind(t, tyT) =>
      TmAbbBind(eval(ctx, t), tyT)
    case b =>
      b
  }
}

