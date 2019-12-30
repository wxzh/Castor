package examples

@family trait Term {
  @adt trait Tm
  @default(Tm) trait Eval1 {
    type OTm = Tm
    def tm = _ => throw NoRuleApplies
  }
}

@family trait Nat extends Term {
  @adt trait Tm extends super.Tm {
    case object TmZero
    case class TmSucc(t: Tm)
    case class TmPred(t: Tm)
  }
  def nv(t: Tm): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => nv(t1)
    case _ => false
  }
  @default(Tm) trait Eval1 extends super.Eval1 {
    override def tmSucc = x => TmSucc(this(x.t))
    override def tmPred = {
      case TmPred(TmZero) => TmZero
      case TmPred(TmSucc(t)) if nv(t) => t
      case TmPred(t) => TmPred(this(t))
    }
  }
}

@family trait Bool extends Term {
  @adt trait Tm extends super.Tm {
    case object TmTrue
    case object TmFalse
    case class TmIf(t1: Tm, t2: Tm, t3: Tm)
  }
  @default(Tm) trait Eval1 extends super.Eval1 {
    override def tmIf = {
      case TmIf(TmTrue,t2,_) => t2
      case TmIf(TmFalse,_,t3) => t3
      case TmIf(t1,t2,t3) => TmIf(this(t1),t2,t3)
    }
  }
}

@family trait Arith extends Nat with Bool {
  @adt trait Tm extends super[Nat].Tm with super[Bool].Tm {
    case class TmIsZero(t: Tm)
  }
  @visit(Tm) trait Eval1 extends super[Nat].Eval1
    with super[Bool].Eval1 {
    def tmIsZero = {
      case TmIsZero(TmZero) => TmTrue
      case TmIsZero(TmSucc(t)) if nv(t) => TmFalse
      case TmIsZero(t) => TmIsZero(this(t))
    }
  }
}


object TestArith extends App {
  import Arith._
  val term = TmIsZero(TmIf(TmFalse,TmTrue,TmPred(TmSucc(TmZero))))
  println(eval1(term))               // TmIsZero(TmPred(TmSucc(TmZero)))
  println(eval1(eval1(term)))        // TmIsZero(TmZero)
  println(eval1(eval1(eval1(term)))) // TmTrue
}

//@family @adts(Tm) @ops(Eval1)
//trait EqArith extends Arith {
//  @visit(Tm) trait Equal {
//    type OTm = Tm => Boolean
//    def tmZero = { case TmZero => true }
//    def tmSucc = t => { case TmSucc(s) => this(t)(s) }
//    def tmPred = t => { case TmPred(s) => this(t)(s) }
//    def tmTrue = { case TmTrue => true }
//    def tmFalse = { case TmFalse => true }
//    def tmIf = (t1,t2,t3) => {
//      case TmIf(s1,s2,s3) => this(t1)(s1) && this(t2)(s2) && this(t3)(s3)
//    }
//    def tmIsZero = t => { case TmIsZero(s) => this(t)(s) }
//    override def apply(t1: Tm) = t2 => {
//      try { t1.accept(this)(t2) }
//      catch { case _: MatchError => false }
//    }
//  }
//}

@family @adts(Tm) @ops(Eval1)
trait EqArith extends Arith {
  @visit(Tm) trait Equal {
    type OTm = Tm => Boolean
    def tmZero = {
      case TmZero => true
      case _ => false
    }
    def tmSucc = x => {
      case TmSucc(t) => this(t)(x.t)
      case _ => false
    }
    def tmPred = x => {
      case TmPred(t) => this(t)(x.t)
      case _ => false
    }
    def tmTrue = {
      case TmTrue => true
      case _ => false
    }
    def tmFalse = {
      case TmFalse => true
      case _ => false
    }
    def tmIf = x => {
      case TmIf(t1,t2,t3) => this(x.t1)(t1) && this(x.t2)(t2) && this(x.t3)(t3)
      case _ => false
    }
    def tmIsZero = x => {
      case TmIsZero(t) => this(x.t)(t)
      case _ => false
    }
  }
}

object TestEqArith extends App {
  import EqArith._
  val t = TmSucc(TmZero)
  println(equal(t)(TmZero)) // false
  println(equal(t)(t))      // true
}

@family @adts(Tm) @ops(Eval1)
trait TyArith extends Arith {
  @adt trait Ty {
    case object TyNat
    case object TyBool
  }
  @visit(Tm) trait Typeof {
    type OTm = Option[Ty]
    def tmZero = Some(TyNat)
    def tmSucc = x => this(x.t) match {
      case ty@Some(TyNat) => ty
      case _ => None
    }
    def tmPred = x => this(x.t) match {
      case ty@Some(TyNat) => ty
      case _ => None
    }
    def tmTrue = Some(TyBool)
    def tmFalse = Some(TyBool)
    def tmIf = x => (this(x.t1),this(x.t2),this(x.t3)) match {
      case (Some(TyBool),o2,o3) if o2==o3 => o2
      case _ => None
    }
    def tmIsZero = x => this(x.t) match {
      case Some(TyNat) => Some(TyBool)
      case _ => None
    }
  }
}

@adts(Tm) @ops(Eval1)
@family trait EvalArith extends Arith {
  @adt trait Value {
    case class IntValue(v: Int)
    case class BoolValue(v: Boolean)
    case object NoValue
  }
  @visit(Tm) trait Eval {
    type OTm = Value
    def tmZero = IntValue(0)
    def tmSucc = x => this(x.t) match {
      case IntValue(n) => IntValue(n+1)
      case _ => throw NoRuleApplies
    }

    def tmPred = x => this(x.t) match {
      case IntValue(n) => IntValue(n-1)
      case _ => throw NoRuleApplies
    }
    def tmTrue = BoolValue(true)
    def tmFalse = BoolValue(false)
    def tmIf = x => this(x.t1) match {
      case BoolValue(true) => this(x.t2)
      case BoolValue(false) => this(x.t3)
      case _ => throw NoRuleApplies
    }
    def tmIsZero = x => this(x.t) match {
      case IntValue(0) => BoolValue(true)
      case IntValue(_) => BoolValue(false)
      case _ => throw NoRuleApplies
    }
  }
}

object TestTyArith extends App {
  import TyArith._
  println(typeof(TmSucc(TmTrue)))
  println(typeof(TmSucc(TmZero)))
}

@family @adts(Tm) @ops(Eval1)
trait PrintArith extends Arith {
  @default(Tm) trait PtmTerm {
    type OTm = String
    def tm = x => ptmAppTerm(x)
    override def tmIf = x =>
      "if " + this(x.t1) + " then " + this(x.t2) + " else " + this(x.t3)
  }
  @default(Tm) trait PtmAppTerm {
    type OTm = String
    def tm = x => ptmATerm(x)
    override def tmPred = x => "pred " + ptmATerm(x.t)
    override def tmSucc = x => "succ " + ptmATerm(x.t)
    override def tmIsZero = x => "iszero " + ptmATerm(x.t)
  }
  @default(Tm) trait PtmATerm {
    type OTm = String
    def tm = x => "(" + ptmTerm(x) + ")"
    override def tmZero = "0"
    override def tmTrue = "true"
    override def tmFalse = "false"
  }
}

object TestPrintArith extends App {
  import PrintArith._
  println(ptmTerm(TmSucc(TmZero))) // succ 0
  val term = TmIsZero(TmIf(TmFalse,TmTrue,TmPred(TmSucc(TmZero))))
  println(ptmTerm(term)) // iszero(if false then true else pred(succ 0))
}

