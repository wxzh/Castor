package tapl.fullsub

import examples._
import tapl.extracted._

@family
@adts(Binding,Tm)
@ops(BindingShift,PBinding,PBindingTy,GetTypeFromBind,CheckBinding,Eval1,IsVal,PtmTerm,PtmATerm,PtmAppTerm,PtmPathTerm,PtmAscribeTerm,TmMap)
trait FullSub extends TopJoinMeet with MoreExt {
  @adt trait Ty extends super[TopJoinMeet].Ty with super[MoreExt].Ty
  @visit(Ty) trait PtyType extends super[TopJoinMeet].PtyType with super[MoreExt].PtyType
  @visit(Ty) trait PtyArrowType extends super[TopJoinMeet].PtyArrowType with super[MoreExt].PtyArrowType
  @visit(Ty) trait PtyAType extends super[TopJoinMeet].PtyAType with super[MoreExt].PtyAType
  @visit(Ty) trait TyEqv extends super[TopJoinMeet].TyEqv with super[MoreExt].TyEqv
  @visit(Ty) trait Subtype extends super[TopJoinMeet].Subtype with super[MoreExt].Subtype
  @default(Ty) trait Join extends super[TopJoinMeet].Join {
    override def tyRecord = fS => {
      case TyRecord(fT) =>
        val labelS = fS.map { _._1 }
        val labelT = fT.map { _._1 }
        val commonLabels = labelS intersect labelT
        val commonFs = commonLabels.map { li =>
          val (_, tySi) = fS.find { _._1 == li }.get
          val (_, tyTi) = fT.find { _._1 == li }.get
          (li, this(tySi)(tyTi))
        }
        TyRecord(commonFs)
    }
  }
  @default(Ty) trait Meet extends super[TopJoinMeet].Meet {
    override def tyRecord = fS => {
      case TyRecord(fT) =>
        val labelS = fS.map { _._1 }
        val labelT = fT.map { _._1 }
        val allLabels = (labelS union labelT).distinct
        // there was an error by Pierce!!
        val allFs = allLabels.flatMap { li =>
          (fS.find { _._1 == li }, fT.find { _._1 == li }) match {
            case (Some((_, tySi)), Some((_, tyTi))) =>
              Some(li -> this(tySi)(tyTi))
            case (Some((_, tySi)), _) =>
              Some(li -> tySi)
            case (_, Some((_, tySi))) =>
              Some(li -> tySi)
            case (None, None) =>
              None
          }
        }
        TyRecord(allFs)
    }
  }
  @visit(Tm) trait Typeof extends super[TopJoinMeet].Typeof with super[MoreExt].Typeof {
    override def tmIf = (t1,t2,t3) => ctx =>
    if (subtype(typeof(t1)(ctx))(TyBool)) {
      join(typeof(t2)(ctx))(this(t3)(ctx))
    } else {
      throw new Exception("guard of conditional " + t1 + " is not a boolean")
    }
  }
}

//  trait PtyType extends top_PtyType with extension_PtyType {_: TyV => }
//  trait PtyArrowType extends top_PtyArrowType with extension_PtyArrowType {_: TyV => }
//  trait PtyAType extends top_PtyAType with extension_PtyAType {_: TyV => }
//
//  trait TyEqv extends top_TyEqv with extension_TyEqv {_: TyV => }
//
////  trait TyMap extends top_TyMap with super[tyextension].TyMap {_: TyV => }
//
//  trait Subtype extends top_Subtype with extension_Subtype {_: TyV =>
////    override def apply(ty1: Ty) = (ty2,ctx) =>
////      super.apply(ty1)(ty2,ctx) || (simplifyTy(ctx,ty2) match {
////        case TyTop => true
////        case _ => false
////      })
//  }
//
//  trait Join extends extension_TyVisitor with top_Join {_: TyV =>
//  }
//
//  // raise MatchError
//  trait Meet extends extension_TyVisitor with top_Meet {_: TyV =>
//  }
//
////  //TODO: called after getBinding
////  trait BindingShift extends BindDefault with super.BindingShift {_: BindV =>
////    override def tmAbbBind = (t, ty) => {
////      val ty1 = ty.map(typeShift(d, _))
////      TmAbbBind(termShift(d, t), ty1)
////    }
////    override def varBind = ty => VarBind(typeShift(d, ty))
////    override def tyAbbBind = ty => TyAbbBind(typeShift(d, ty))
////  }
////
////
////  trait PBinding extends BindDefault with super.PBinding {_: BindV =>
////    override def tmAbbBind = (t, _) =>
////      "= " :: ptm(t)
////    override def tyAbbBind =
////      "= " :: ptyTy(_)
////  }
////
////  trait PBindingTy extends PBinding {_: BindV =>
////    override def tmAbbBind = {
////      case (_, Some(ty)) => ": " :: ptyTy(ty)
////      case (t, None) => ": " :: ptyTy(typeof(t)(ctx))
////    }
////    override def tyAbbBind = _ =>
////      ":: *"
////  }
////
////  trait GetTypeFromBind extends BindDefault with super.GetTypeFromBind {_: BindV =>
////    override def tmAbbBind = {
////      case (_, Some(ty)) => ty
////      case (_, None) => throw new Exception("No type recorder for variable")
////    }
////  }
////
////  trait CheckBinding extends BindDefault {_: BindV =>
////    type T = Binding
////    val ctx: Context
////    def otherwise = identity
////    def tmAbbBind = {
////      case (t, None) => TmAbbBind(t, Some(typeof(t)(ctx))
////      case (t, Some(ty)) =>
////        val tyT = typeof(t)(ctx)
////        if (tyEqv(ctx)(t1)(t))
////          TmAbbBind(t, Some(ty))
////        else
////          throw new Exception("type of binding doesn't match declared type")
////    }
////  }
//}
//
//object fullsub extends fullsub {
//  type TmV = extension_TmVisitor
//  type TyV = extension_TyVisitor
//  type BindV = BindVisitor
//
//  object ptmTerm extends extension_PtmTerm
//  object ptmPathTerm extends extension_PtmPathTerm
//  object ptmAscribeTerm extends extension_PtmAscribeTerm
//  object ptmAppTerm extends extension_PtmAppTerm
//  object ptmATerm extends extension_PtmATerm
//
//  object ptyType extends PtyType
//  def ptyAType(c: Context) =  new PtyAType{val ctx=c}
//  val ptyAType: PtyAType = ptyAType(Context())
//  object ptyArrowType extends PtyArrowType {_: TyV =>}
//
//  object subtype extends Subtype
//  object tyEqv extends TyEqv
//
//
//  object eval1 extends Eval1
//  object isVal extends IsVal
//  object typeof extends Typeof
//  def tyMap(f: (Int,Int,Int) => Ty) = new TyMap {val onVar=f}
//  def tmMap(f: (Int,Int,Int) => Tm, g: (Int,Ty) => Ty) = new TmMap {val onVar=f; val onType=g}
////  def tmMap(f: (Int,Int,Int) => Tm): TmMap = tmMap(f,null)
//  object join extends Join
//  object meet extends Meet
//
//  def pBindingTy(c: Context) = new PBindingTy {val ctx=c}
//  object getTypeFromBind extends GetTypeFromBind
//}
