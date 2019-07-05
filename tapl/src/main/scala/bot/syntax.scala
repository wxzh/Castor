package tapl.bot

import tapl.extracted._
import util.Print._
import examples._

@adts(Binding,Tm)
@ops(BindingShift, PBinding, GetTypeFromBind, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap)
@family
trait Bot extends Top {
  @adt trait Ty extends super.Ty {
    case object TyBot
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
    override def tmApp = x => ctx => {
      this(x.t1)(ctx) match {
        case TyBot => TyBot
        case _ => super.tmApp(x)(ctx)
      }
    }
  }
}

@family
@adts(Binding,Tm,Ty)
@ops(BindingShift, PBinding, GetTypeFromBind, Eval1, IsVal, PtmTerm, PtmATerm, PtmAppTerm, TmMap, Typeof, PtyType, PtyAType, PtyArrowType, Subtype, TyEqv)
trait BotJoinMeet extends TopJoinMeet with Bot {
  @default(Ty) trait Join extends super.Join
  @default(Ty) trait Meet extends super.Meet {
    override def default = TyBot
  }
}
