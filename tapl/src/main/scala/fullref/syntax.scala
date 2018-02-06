//package tapl.fullref
//
//import examples._
//import tapl.extracted._
//import tapl.fullsimple._
//import util.Print._
//import util.Document._
//import tapl.bot._
//
//@family
//@adts(Binding)
//@ops(BindingShift,PBinding,PBindingTy,GetTypeFromBind,CheckBinding)
//trait FullRef extends FullSimple with BotJoinMeet {
//  @adt trait Tm extends super[FullSimple].Tm {
//    def TmLoc: Int => Tm
//    def TmRef: Tm => Tm
//    def TmDeref: Tm => Tm
//    def TmAssign: (Tm,Tm) => Tm
//  }
//
//  @adt trait Ty extends super[FullSimple].Ty with super[BotJoinMeet].Ty {
//    def TyRef: Ty => Ty
//    def TySource: Ty => Ty
//    def TySink: Ty => Ty
//  }
//
//  @visit(Tm) trait IsVal extends super[FullSimple].IsVal {
//    def tmLoc = _ => true
//    def tmRef = _ => false
//    def tmDeref = _ => false
//    def tmAssign = (_,_) => false
//  }
//
//  case class Store(l: List[Tm] = List()) {
//    def extend(v: Tm): (Int, Store) =
//      (l.length, Store(l :+ v))
//    def lookup(i: Int): Tm =
//      l(i)
//    def update(n: Int, v: Tm): Store =
//      Store(l.updated(n, v))
//    def shift(i: Int): Store =
//      Store(l.map(termShift(i,_)))
//  }
//
//  @default(Tm) trait PtmTerm extends super[FullSimple].PtmTerm {
//    override def tmAssign = (t1,t2) => (_,ctx) =>
//      g2(ptmAppTerm(t1)(false, ctx) :/: ":=" :/: ptmAppTerm(t2)(false, ctx))
//  }
//
//  @default(Tm) trait PtmAppTerm extends super[FullSimple].PtmAppTerm {
//    override def tmRef = t => (_,ctx) =>
//      "ref " :: ptmATerm(t)(false, ctx)
//    override def tmDeref = t => (_,ctx) =>
//      "!" :: ptmATerm(t)(false, ctx)
//  }
//
//  @default(Tm) trait PtmATerm extends super[FullSimple].PtmATerm {
//    override def tmLoc = l => (_,_) =>
//      "<loc #" + l + ">"
//  }
//
//  @default(Tm) trait PtmAscribeTerm extends super[FullSimple].PtmAscribeTerm
//
//  @default(Tm) trait PtmPathTerm extends super[FullSimple].PtmPathTerm
//
//  @visit(Tm) trait TmMap extends super[FullSimple].TmMap {
//    def tmLoc = l => (_,_) => TmLoc(l)
//    def tmRef = t => (c,onvar) => TmRef(this(t)(c,onvar))
//    def tmDeref = t => (c,onvar) => TmDeref(this(t)(c,onvar))
//    def tmAssign = (t1,t2) => (c,onvar) => TmAssign(this(t1)(c,onvar), this(t2)(c,onvar))
//  }
//
////  @visit(Tm) trait Eval1 extends super[FullSimple].Eval1 {
////    type OTm = Store => (Tm,Store)
////    def tmRef = t => store =>
////      if (!isVal(t)) {
////        val (t1,store1) = this(t)(store)
////        (TmRef(t1),store1)
////      } else {
////        val (l,store1) = store.extend(t)
////        (TmLoc(l),store)
////      }
////    def tmDeref = t => store =>
////      if (!isVal(t)) {
////        val (t1,store1) = this(t)(store)
////        (TmDeref(t1), store1)
////      } else {
////        t match {
////          case TmLoc(l) => (store.lookup(l), store)
////          case _ => throw NoRuleApplies()
////        }
////      }
////    def tmAssign = (t1,t2) => store =>
////      if (!isVal(t1)) {
////        val (t11,store1) = this(t1)(store)
////        (TmAssign(t11, t2), store1)
////      } else if (!isVal(t2)) {
////        val (t21, store1) = this(t2)(store)
////        (TmAssign(t1, t21), store1)
////      } else {
////        t1 match {
////          case TmLoc(l) => (TmUnit, store.update(l,t2))
////          case _ => throw NoRuleApplies()
////        }
////      }
////  }
//
//  @visit(Tm) trait Typeof extends super[FullSimple].Typeof {
//    def tmRef = t => ctx =>
//      TyRef(this(t)(ctx))
//    def tmLoc = l =>
//      throw new Exception("locations are not supposed to occur in source programs!")
//    def tmDeref = t => ctx => this(t)(ctx) match {
//      case TyRef(ty) => ty
//      case TyBot => TyBot
//      case TySource(ty) => ty
//      case _ => throw new Exception("argument of ! is not a Ref")
//    }
//    def tmAssign = (t1,t2) => ctx => this(t1)(ctx) match {
//      case TyRef(ty) =>
//        if (subtype(this(t2)(ctx))(ty))
//          TyUnit
//        else
//          throw new Exception("arguments of := are incompatible")
//      case TySink(ty) =>
//        if (subtype(this(t2)(ctx))(ty))
//          TyUnit
//        else
//          throw new Exception("arguments of := are incompatible")
//      case TyBot =>
//        this(t2)(ctx)
//        TyBot
//    }
//  }
//
//  @visit(Ty) trait PtyType extends super[FullSimple].PtyType with super[BotJoinMeet].PtyType {
//    def tyRef = ty => (_,ctx) => "Ref " :: ptyAType(ty)(false,ctx)
//    def tySource = ty => (_,ctx) => "Source " :: ptyAType(ty)(false,ctx)
//    def tySink = ty => (_,ctx) => "Sink " :: ptyAType(ty)(false,ctx)
//  }
//
//  @default(Ty) trait PtyAType extends super[FullSimple].PtyAType with super[BotJoinMeet].PtyAType
//
//  @default(Ty) trait PtyArrowType extends super[FullSimple].PtyArrowType with super[BotJoinMeet].PtyArrowType
//
//  @visit(Ty) trait TyEqv extends super[FullSimple].TyEqv with super[BotJoinMeet].TyEqv {
//    def tyRef = ty1 => { case TyRef(ty2) => this(ty1)(ty2) }
//    def tySource = ty1 => { case TySource(ty2) => this(ty1)(ty2) }
//    def tySink = ty1 => { case TySink(ty2) => this(ty1)(ty2) }
//  }
//
//  @visit(Ty) trait Subtype extends super[FullSimple].Subtype with super[BotJoinMeet].Subtype {
//    def tyRef = ty1 => {
//      case TyRef(ty2) => this(ty1)(ty2) && this(ty2)(ty1)
//      case TySource(ty2) => this(ty1)(ty2)
//      case TySink(ty2) => this(ty1)(ty2)
//    }
//    def tySource = ty1 => { case TySource(ty2) => this(ty1)(ty2) }
//    def tySink = ty1 => { case TySink(ty2) => this(ty1)(ty2) }
//  }
//
//  @default(Ty) trait Join extends super[BotJoinMeet].Join {
//    override def tyRef = ty1 => {
//      case TyRef(ty2) =>
//        if (subtype(ty1)(ty2) && subtype(ty2)(ty1))
//          TyRef(ty1)
//        else
//          TySource(this(ty1)(ty2))
//      case TySource(ty2) =>
//        TySource(join(ty1)(ty2))
//      case TySink(ty2) =>
//        TySink(meet(ty1)(ty2))
//    }
//    override def tySource = ty1 => {
//      case TySource(ty2) =>
//        TySource(this(ty1)(ty2))
//      case TyRef(ty2) =>
//        TySource(this(ty1)(ty2))
//    }
//    override def tySink = ty1 => {
//      case TySink(ty2) =>
//        TySink(meet(ty1)(ty2))
//      case TyRef(ty2) =>
//        TySink(meet(ty1)(ty2))
//    }
//  }
//
//  @default(Ty) trait Meet extends super[BotJoinMeet].Meet {
//    override def tyRef = ty1 => {
//      case TyRef(ty2) =>
//        if (subtype(ty1)(ty2) && subtype(ty2)(ty1))
//          TyRef(ty1)
//        else
//          TySource(this(ty1)(ty2))
//      case TySource(ty2) =>
//        TySource(this(ty1)(ty2))
//      case TySink(ty2) =>
//        TySink(this(ty1)(ty2))
//    }
//    override def tySource = ty1 => {
//      case TySource(ty2) =>
//        TySource(this(ty1)(ty2))
//      case TyRef(ty2) =>
//        TySource(join(ty1)(ty2))
//    }
//    override def tySink = ty1 => {
//      case TySink(ty2) =>
//        TySink(this(ty1)(ty2))
//      case TyRef(ty2) =>
//        TySink(this(ty1)(ty2))
//    }
//  }
//}
