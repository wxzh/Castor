package tapl.bot

import util.Print._
import tapl.simplebool.Typed
import examples._


@adts(Binding,Tm)
@ops(BindingShift, PBinding, GetTypeFromBind, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap)
@vicase
trait Top extends Typed {
  @adt trait Ty extends super.Ty {
    def TyTop: Ty
  }

  @default(Ty) trait PtyType extends super.PtyType
  @default(Ty) trait PtyArrowType extends super.PtyArrowType
  @visit(Ty) trait PtyAType extends super.PtyAType {
    def tyTop = (_,_) => "Top"
  }

  @visit(Tm) trait Typeof extends super.Typeof {
    override def tmApp = (t1, t2) => ctx => {
      val ty1 = this(t1)(ctx)
      val ty2 = this(t2)(ctx)
      ty1 match {
        case TyArr(ty11,ty12) =>
          if (subtype(ty2)(ty11)) ty12
          else throw new Exception("parameter mismatch" + " : " + ty2 + " != " + ty11)
        case _ => throw new Exception("arrow type expected")
      }
    }
  }

  @visit(Ty) trait Subtype extends super.Subtype with TyEqv {
    override def apply(ty1: Ty) = ty2 =>
      (ty1 == ty2) || (ty2 match {
        case TyTop => true
        case _ => ty1.accept(this)(ty2)
      })
  }

  @visit(Ty) trait TyEqv extends super.TyEqv {
    def tyTop = { case TyTop => true } // BUG: a missing case in Ilya's impl
  }
}

@adts(Binding,Tm)
@ops(BindingShift, PBinding, GetTypeFromBind, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap)
@vicase
trait Bot extends Top {
  @adt trait Ty extends super.Ty {
    def TyBot: Ty
  }

  @default(Ty) trait PtyType extends super.PtyType

  @default(Ty) trait PtyArrowType extends super.PtyArrowType

  @visit(Ty) trait PtyAType extends super.PtyAType {
    def tyBot = (_,_) => "Bot"
  }

  @visit(Ty) trait Subtype extends super.Subtype {
    def tyBot = _ => true
  }

  @visit(Ty) trait TyEqv extends super.TyEqv {
    def tyBot = { case TyBot => true }
  }

  @visit(Tm) trait Typeof extends super.Typeof {
    override def tmApp = (t1,t2) => ctx => {
      this(t1)(ctx) match {
        case TyBot => TyBot
        case _ => super.tmApp(t1, t2)(ctx)
      }
    }
  }
}
//
//trait botjoinmeet extends tapl.extracted.topjoinmeet with bot {
//  trait bot_Join extends bot_TyDefault with top_Join {_: TyV => }
//  trait bot_Meet extends bot_TyDefault with top_Meet {_: TyV =>
//    override def default = TyBot
//  }
//}