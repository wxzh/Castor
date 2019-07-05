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
    case class TmFix(t: Tm)
    case object TmUnit
    case class TmAscribe(t: Tm, ty: Ty)
    case class TmInert(ty: Ty)
  }

  @adt trait Ty extends super.Ty {
    case object TyUnit
  }

  @visit(Tm) trait TmMap extends super.TmMap {
    def tmFix = x => (onvar,c) => TmFix(this(x.t)(onvar,c))
    def tmAscribe = x => (onvar,c) => TmAscribe(this(x.t)(onvar,c),x.ty)
    def tmUnit = (_,_) => TmUnit
    def tmInert = x => (_,_) => x
  }

  @default(Tm) trait PtmTerm extends super.PtmTerm {
    override def tmFix = x => (_,ctx) =>
      g2("fix " :: this(x.t)(false,ctx))
    override def tmAbs = super.tmAbs
    override def tmLet = super.tmLet
  }

  @default(Tm) trait PtmAppTerm extends super.PtmAppTerm

  @default(Tm) trait PtmATerm extends super.PtmATerm {
    override def tmUnit = (_,_) => "unit"
    override def tmInert = x => (_,ctx) =>
      "inert[" :: ptyType(x.ty)(false, ctx) :: "]"
    override def tmString = super.tmString
  }

  @default(Tm) trait PtmPathTerm extends super.PtmPathTerm {
    override def tm = ptmAscribeTerm(_)
  }

  @default(Tm) trait PtmAscribeTerm {
    type OTm = (Boolean,Context) => Document
    def tm = ptmATerm(_)
    override def tmAscribe = x => (outer,ctx) =>
      g0(ptmAppTerm(x.t)(false,ctx) :/: "as " :: ptyType(x.ty)(false,ctx))
  }

  @visit(Tm) trait Eval1 extends super.Eval1 {
    def tmFix = x => ctx =>
      if (isVal(x.t)) {
        x.t match {
          case TmAbs(_, _, t12) =>
            termSubstTop(x, t12)
          case _ => throw NoRuleApplies()
        }
      }
      else
        TmFix(this(x.t)(ctx))
    def tmAscribe = x => ctx =>
      if (isVal(x.t)) x.t
      else TmAscribe(this(x.t)(ctx),x.ty)
    def tmUnit = tm(TmUnit)
    def tmInert = tm

  }

  @visit(Tm) trait Typeof extends super.Typeof {
    def tmFix = x => ctx => {
      this(x.t)(ctx) match {
        case TyArr(ty1,ty2) =>
          if (ty1 == ty2)
            ty2
          else {
            throw new Exception(s"result of body not compatible with domain ${x.t}: $ty2 != $ty1")
          }
        case _ =>
          throw new Exception("error type expected in " + x.t)
      }
    }
    def tmUnit = _ => TyUnit
    def tmInert = x => _ => x.ty
    def tmAscribe = x => ctx =>
      if (subtype(this(x.t)(ctx))(x.ty))
        x.ty
      else
        throw new Exception("body of as-term doesn't have the expected type in " + x.t)

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
    def tmAscribe = _ => false
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

