package examples

object NoRuleApplies extends RuntimeException
object Visitor {

// Class hierarchy
abstract class Tm {
  def accept[A](v: TmVisit[A]): A
}
class TmZero() extends Tm {
  def accept[A](v: TmVisit[A]) = v.tmZero(this)
}
class TmSucc(val t: Tm) extends Tm {
  def accept[A](v: TmVisit[A]) = v.tmSucc(this)
}
class TmPred(val t: Tm) extends Tm {
  def accept[A](v: TmVisit[A]) = v.tmPred(this)
}
class TmTrue() extends Tm {
  def accept[A](v: TmVisit[A]): A = v.tmTrue(this)
}
class TmFalse() extends Tm {
  def accept[A](v: TmVisit[A]): A = v.tmFalse(this)
}
class TmIf(val t1: Tm, val t2: Tm, val t3: Tm) extends Tm {
  def accept[A](v: TmVisit[A]): A = v.tmIf(this)
}
class TmIsZero(val t: Tm) extends Tm {
  def accept[A](v: TmVisit[A]): A = v.tmIsZero(this)
}

// Visitor interface
trait TmVisit[A] {
  def tmZero(x: TmZero): A
  def tmSucc(x: TmSucc): A
  def tmPred(x: TmPred): A
  def tmTrue(x: TmTrue): A
  def tmFalse(x: TmFalse): A
  def tmIf(x: TmIf): A
  def tmIsZero(x: TmIsZero): A
}

// Numeric value checking visitor
class Nv extends TmVisit[Boolean] {
  def tmZero(x: TmZero) = true
  def tmSucc(x: TmSucc)= x.t.accept(this)
  def tmPred(x: TmPred) = false
  def tmTrue(x: TmTrue) = false
  def tmFalse(x: TmFalse) = false
  def tmIf(x: TmIf) = false
  def tmIsZero(x: TmIsZero) = false
}

// Small-step evaluation visitor
class Eval1 extends TmVisit[Tm] { eval1 =>
  val nv = new Nv // Dependency
  def tmZero(x: TmZero) = throw NoRuleApplies
  def tmSucc(x: TmSucc) = new TmSucc(x.t.accept(this))
  def tmPred(x: TmPred) = x.t.accept(new TmVisit[Tm] { // Anonymous visitor
    def tmZero(y: TmZero) = y                 // PredZero
    def tmSucc (y: TmSucc) =
      if (y.t.accept(nv)) y.t                               // PredSucc
      else new TmPred(y.t.accept(eval1))                    // Pred
    def tmPred(y: TmPred) = new TmPred(y.accept(eval1))     // Pred
    def tmTrue(y: TmTrue) = new TmPred(y.accept(eval1))     // Pred
    def tmFalse(y: TmFalse) = new TmPred(y.accept(eval1))   // Pred
    def tmIf(y: TmIf) = new TmPred(y.accept(eval1))         // Pred
    def tmIsZero(y: TmIsZero) = new TmPred(y.accept(eval1)) // Pred
  })
  def tmTrue(x: TmTrue) = throw NoRuleApplies
  def tmFalse(x: TmFalse)  = throw NoRuleApplies
  def tmIf(x: TmIf) = x.t1.accept(new TmVisit[Tm] {
    def tmTrue(y: TmTrue) = x.t2
    def tmFalse(y: TmFalse) = x.t3
    def tmZero(y: TmZero) = new TmIf(y.accept(eval1),x.t2,x.t3)
    def tmSucc(y: TmSucc) = new TmIf(y.accept(eval1),x.t2,x.t3)
    def tmPred(y: TmPred) = new TmIf(y.accept(eval1),x.t2,x.t3)
    def tmIf(y: TmIf) = new TmIf(y.accept(eval1),x.t2,x.t3)
    def tmIsZero(y: TmIsZero) = new TmIf(y.accept(eval1),x.t2,x.t3)
  })
  def tmIsZero(x: TmIsZero) = x.t.accept(new TmVisit[Tm] {
    def tmZero(y: TmZero) = new TmTrue
    def tmSucc (y: TmSucc) =
      if (y.t.accept(nv)) new TmFalse
      else new TmIsZero(y.accept(eval1))
    def tmPred(y: TmPred) = new TmIsZero(y.accept(eval1))
    def tmTrue(y: TmTrue) = new TmIsZero(y.accept(eval1))
    def tmFalse(y: TmFalse) = new TmIsZero(y.accept(eval1))
    def tmIf(y: TmIf) = new TmIsZero(y.accept(eval1))
    def tmIsZero(y: TmIsZero) = new TmIsZero(y.accept(eval1))
  })
}
}

object TestVisitor extends App {
  import Visitor._
  val term = new TmIsZero(new TmIf(new TmFalse,new TmTrue,new TmPred(new TmSucc(new TmZero))))
  val eval1 = new Eval1
  val nv = new Nv
  println(new TmIf(new TmTrue,new TmTrue,new TmFalse).accept(eval1).isInstanceOf[TmTrue])
//
//  println(new TmIsZero (new TmSucc(new TmZero)).accept(eval1).isInstanceOf[TmTrue])
//    println(new TmIf(new TmFalse,new TmTrue,new TmPred(new TmSucc(new TmZero))).accept(eval1).isInstanceOf[TmIsZero])
  println(term.accept(eval1))
  println(term.accept(eval1).accept(eval1))
  println(term.accept(eval1).accept(eval1).accept(eval1))
}

object Sealed {
sealed abstract class Tm
case object TmZero extends Tm
case class TmSucc(t: Tm) extends Tm
case class TmPred(t: Tm) extends Tm

def nv(t: Tm): Boolean = t match {
  case TmZero => true
  case TmSucc(t1) => nv(t1)
  case _ => false
}

def eval1(t: Tm): Tm = t match {
  case TmSucc(t1) => TmSucc(eval1(t1))
  case TmPred(TmZero) => TmZero
  case TmPred(TmSucc(t1)) if nv(t1) => t1
  case TmPred(t1) => TmPred(eval1(t1))
  case _ => throw NoRuleApplies
}
}

object Open {
trait Term {
  trait Tm
  def eval1(tm: Tm): Tm = throw NoRuleApplies
}
trait Nat extends Term {
  case object TmZero extends Tm
  case class TmSucc(t: Tm) extends Tm
  case class TmPred(t: Tm) extends Tm
  def nv(t: Tm): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => nv(t1)
    case _ => false
  }
  override def eval1(t: Tm): Tm = t match {
    case TmSucc(t1) => TmSucc(eval1(t1))
    case TmPred(TmZero) => TmZero
    case TmPred(TmSucc(nv1)) if nv(nv1) => nv1
    case TmPred(t1) => TmPred(eval1(t1))
    case _ => super.eval1(t)
  }
}

trait Bool extends Term {
  case object TmTrue extends Tm
  case object TmFalse extends Tm
  case class TmIf(t1: Tm, t2: Tm, t3: Tm) extends Tm
  override def eval1(t: Tm): Tm = t match {
    case TmIf(TmTrue,t2,_) => t2
    case TmIf(TmFalse,_,t3) => t3
    case TmIf(t1,t2,t3) => TmIf(eval1(t1),t2,t3)
    case _ => super.eval1(t)
  }
}

trait Arith extends Nat with Bool {
  case class TmIsZero(t: Tm) extends Tm
  override def eval1(t: Tm): Tm = t match {
    case TmIsZero(TmZero) => TmTrue
    case TmIsZero(TmSucc(t1)) if nv(t1) =>
      TmFalse
    case TmIsZero(t1) => TmIsZero(eval1(t1))
    case TmZero => super[Nat].eval1(t)
    case _: TmSucc => super[Nat].eval1(t)
    case _: TmPred => super[Nat].eval1(t)
    case _ => super[Bool].eval1(t)
  }
}
}

object Partial {
trait Term {
  trait Tm
  def eval1: PartialFunction[Tm,Tm]
}
trait Nat extends Term {
  case object TmZero extends Tm
  case class TmSucc(t: Tm) extends Tm
  case class TmPred(t: Tm) extends Tm
  def nv(t: Tm): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => nv(t1)
    case _ => false
  }
  def eval1: PartialFunction[Tm,Tm] = {
    case TmSucc(t1) => TmSucc(eval1(t1))
    case TmPred(TmZero) => TmZero
    case TmPred(TmSucc(t1)) if nv(t1) => t1
    case TmPred(t1) => TmPred(eval1(t1))
    case TmZero => throw NoRuleApplies
  }
}
trait Bool extends Term {
  case object TmTrue extends Tm
  case object TmFalse extends Tm
  case class TmIf(t1: Tm, t2: Tm, t3: Tm) extends Tm
  def eval1: PartialFunction[Tm,Tm] = {
    case TmIf(TmTrue,t2,_) => t2
    case TmIf(TmFalse,_,t3) => t3
    case TmIf(t1,t2,t3) => TmIf(eval1(t1),t2,t3)
    case TmTrue => throw NoRuleApplies
    case TmFalse => throw NoRuleApplies
  }
}
trait Arith extends Nat with Bool {
  case class TmIsZero(t: Tm) extends Tm
  override def eval1 =
    super[Nat].eval1 orElse super[Bool].eval1 orElse {
      case TmIsZero(TmZero) => TmTrue
      case TmIsZero(TmSucc(t1)) if nv(t1) =>
        TmFalse
      case TmIsZero(t1) => TmIsZero(eval1(t1))
    }
}
}

object Encoding {
trait Term {
  type TmV <: TmVisit
  abstract class Tm {
    def accept(v: TmV): v.OTm
  }
  trait TmVisit { _: TmV =>
    type OTm
    def apply(t: Tm) = t.accept(this)
  }
  trait TmDefault extends TmVisit { _: TmV =>
    def tm: Tm => OTm
  }
  trait Eval1 extends TmDefault { _: TmV =>
    type OTm = Tm
    def tm = _ => throw NoRuleApplies
  }
  val eval1: Eval1
}
trait Nat extends Term {
  type TmV <: TmVisit
  case object TmZero extends Tm {
    def accept(v: TmV): v.OTm = v.tmZero
  }
  case class TmSucc(t: Tm) extends Tm {
    def accept(v: TmV): v.OTm = v.tmSucc(this)
  }
  case class TmPred(t: Tm) extends Tm {
    def accept(v: TmV): v.OTm = v.tmPred(this)
  }
  trait TmVisit extends super.TmVisit { _: TmV =>
    def tmZero: OTm
    def tmSucc: TmSucc => OTm
    def tmPred: TmPred => OTm
  }
  trait TmDefault extends TmVisit with super.TmDefault { _: TmV =>
    def tmZero = tm(TmZero)
    def tmSucc = tm
    def tmPred = tm
  }
  def nv(t: Tm): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => nv(t1)
    case _ => false
  }
  trait Eval1 extends TmDefault with super.Eval1 { _: TmV =>
    override def tmSucc = t => TmSucc(this(t))
    override def tmPred = {
      case TmPred(TmZero) => TmZero
      case TmPred(TmSucc(t)) if nv(t) => t
      case TmPred(t) => TmPred(this(t))
    }
  }
  trait Bool extends Term {
    type TmV <: TmVisit
    trait TmVisit extends super.TmVisit { _: TmV =>
      def tmTrue: OTm
      def tmFalse: OTm
      def tmIf: TmIf => OTm
    }
    trait TmDefault extends TmVisit with super.TmDefault { _: TmV =>
      def tmTrue = tm(TmTrue)
      def tmFalse = tm(TmFalse)
      def tmIf = tm
    }
    case object TmTrue extends Tm {
      override def accept(v: TmV) = v.tmTrue
    }
    case object TmFalse extends Tm {
      override def accept(v: TmV) = v.tmFalse
    }
    case class TmIf(t1: Tm, t2: Tm, t3: Tm) extends Tm {
      override def accept(v: TmV) = v.tmIf(this)
    }
    trait Eval1 extends TmDefault with super.Eval1 { _: TmV =>
      override def tmIf = {
        case TmIf(TmTrue, t2, _) => t2
        case TmIf(TmFalse, _, t3) => t3
        case TmIf(t1, t2, t3) => TmIf(this(t1), t2, t3)
      }
    }
  }

  trait Arith extends Nat with Bool {
    type TmV <: TmVisit
    case class TmIsZero(t: Tm) extends Tm {
      override def accept(v: TmV) = v.tmIsZero(this)
    }
    trait TmVisit extends super[Nat].TmVisit
                  with super[Bool].TmVisit { _: TmV =>
      def tmIsZero: TmIsZero => OTm
    }
    trait TmDefault extends TmVisit with super[Nat].TmDefault
                    with super[Bool].TmDefault { _: TmV =>
      def tmIsZero = tm
    }
    trait Eval1 extends TmVisit with super[Nat].Eval1 with super[Bool].Eval1 { _: TmV =>
      def tmIsZero = {
        case TmIsZero(TmZero) => TmTrue
        case TmIsZero(TmSucc(t)) if nv(t) => TmFalse
        case TmIsZero(t) => TmIsZero(this(t))
      }
    }
  }

  object Arith extends Arith {
    type TmV = TmVisit
    object eval1 extends Eval1
  }
}
}
