package examples

import utils._

  @visitor trait record_TmVis {
    type Tm
    def tmRecord: List[(String,Tm)] => Tm
    def tmProj: (Tm,String) => Tm
  }

  @visitor trait record_TyVis {
    type Ty
    def tyRecord: List[(String,Ty)] => Ty
  }

  trait record_Typeof[Bind] extends record_TmVis { self =>
    type Ty
    type OTm = Context[Bind] => Ty
    val ty: record_TyVis.Fact{ type Ty = self.Ty }
    def tmRecord = fields => ctx =>
      ty.TyRecord(fields.map{case (s,t) => (s,visitTm(t)(ctx))})
    def tmProj = (t,l) => ctx =>
      visitTm(t)(ctx) match {
        case ty.TyRecord(fieldTys) => fieldTys.find{_._1 == l} match {
          case Some((_,ty)) => ty
          case None => throw new Exception(s"Label $l not found")
        }
      }
  }

  trait record_Eval1 extends record_TmVis {
    type OTm = Tm
    val isVal: Tm => Boolean

    def tmRecord = xs => {
      def evalAField(l: List[(String,Tm)]): List[(String,Tm)] = l match {
        case Nil => throw NoRuleApplies
        case (l,v) :: rest  if isVal(v) => (l,v) :: evalAField(rest)
        case (l,t) :: rest => (l,visitTm(t)) :: rest
      }
      TmRecord(evalAField(xs))
    }
    def tmProj = {
      case (v @ TmRecord(fields), l) if isVal(v) => fields.find(_._1 == l) match {
        case Some((_,t)) => t
        case None => throw NoRuleApplies
      }
      case (t,l) => TmProj(visitTm(t),l)
    }
  }

  trait record_TyEqv extends record_TyVis {//with ITyEqv[Ty] {
    type OTy = Ty => Boolean
    def tyRecord = xs => {
      case TyRecord(ys) =>
        xs.length == ys.length && xs.forall{ case (x,t) =>
          ys.exists{ case (y,u) => x == y && apply(t,u) }}
    }
  }

  trait record_Subtype extends record_TyVis {
    type OTy = Ty => Boolean
    def tyRecord = fS => {
      case TyRecord(fT) => fT.forall{ case (x,t) =>
        fS.exists{ case (y,u) => x == y && apply(t,u) }}
      case _ => false
    }
    def apply(ty1: Ty, ty2: Ty): Boolean
  }

  trait record_Join extends record_TyVis {
    type OTy = Ty => Ty
    val subtype: (Ty, Ty) => Boolean
    def tyRecord = fS => {
      case TyRecord(fT) =>
        val labelS = fS.map{_._1}
        val labelT = fT.map{_._1}
        val commonLabels = labelS intersect labelT
        val commonFs = commonLabels.map { l =>
          val (_,tyS) = fS.find {_._1 == l}.get
          val (_,tyT) = fT.find {_._1 == l}.get
          (l, apply(tyS,tyT))
        }
        TyRecord(commonFs)
    }
    def apply(ty1: Ty, ty2: Ty): Ty =
      if (subtype(ty1,ty2)) ty2
      else if (subtype(ty2,ty1)) ty1
      else visitTy(ty1)(ty2)
  }

  trait record_Meet extends record_TyVis {
    type OTy = Ty => Ty
    val subtype: (Ty, Ty) => Boolean
    def tyRecord = fS => {
      case TyRecord(fT) =>
        val labelS = fS.map{_._1}
        val labelT = fT.map{_._1}
        val allLabels = (labelS ++ labelT).distinct
        val allFs = allLabels.flatMap { l =>
          (fS.find {_._1 == l},fT.find {_._1 == l}) match {
            case (Some((_,tyS)),Some((_,tyT))) => Some(l -> apply(tyS,tyT))
            case (Some((_,tyS)),_) => Some(l -> tyS)
            case (_,Some((_,tyT))) => Some(l -> tyT)
            case (None,None) => None
          }
        }
        TyRecord(allFs)
    }
    def apply(ty1: Ty, ty2: Ty): Ty =
      if (subtype(ty1,ty2)) ty1
      else if (subtype(ty2,ty1)) ty2
      else visitTy(ty1)(ty2)
  }
