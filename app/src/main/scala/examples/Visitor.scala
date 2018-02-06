package examples

object Visitor {

abstract class Tm {
  def accept[T](v: TmVisit[T]): T
}
object TmZero extends Tm {
  def accept[T](v: TmVisit[T]) = v.tmZero
}
class TmSucc(t: Tm) extends Tm {
  def accept[T](v: TmVisit[T]) = v.tmSucc(t)
}
class TmPred(t: Tm) extends Tm {
  def accept[T](v: TmVisit[T]) = v.tmPred(t)
}

trait TmVisit[T] {
  def tmZero: T
  def tmSucc(t: Tm): T
  def tmPred(t: Tm): T
}

object nv extends TmVisit[Boolean] {
  def tmZero = true
  def tmSucc(t: Tm) = t.accept(this)
  def tmPred(t: Tm) = false
}

object eval1 extends TmVisit[Tm] {
  def tmZero = throw NoRuleApplies
  def tmSucc(t: Tm) =
    new TmSucc(t.accept(eval1))
  def tmPred(t: Tm) =
    t.accept(new TmVisit[Tm] {
      def tmZero = TmZero
      def tmSucc(t1: Tm) =
        if (t1.accept(nv)) t1
        else new TmPred(t.accept(eval1))
      def tmPred(t1: Tm) = new TmPred(t.accept(eval1))
    })
}
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
abstract class Tm
trait Nat {
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
    case TmPred(TmSucc(nv1)) if nv(nv1) => nv1
    case TmPred(t1) => TmPred(eval1(t1))
    case _ => throw NoRuleApplies
  }
}

trait Bool {
  case object TmTrue extends Tm
  case object TmFalse extends Tm
  case class TmIf(t1: Tm, t2: Tm, t3: Tm) extends Tm
  def eval1(t: Tm): Tm = t match {
    case TmIf(TmTrue,t2,_) => t2
    case TmIf(TmFalse,_,t3) => t3
    case TmIf(t1,t2,t3) => TmIf(eval1(t1),t2,t3)
    case _ => throw NoRuleApplies
  }
}

trait Arith extends Nat with Bool {
  case class TmIsZero(t: Tm) extends Tm
  override def eval1(t: Tm): Tm = t match {
    case _: TmSucc => super[Nat].eval1(t)
    case _: TmPred => super[Nat].eval1(t)
    case _: TmIf => super[Bool].eval1(t)
    case TmIsZero(TmZero) => TmTrue
    case TmIsZero(TmSucc(t1)) if nv(t1) =>
      TmFalse
    case TmIsZero(t1) => TmIsZero(eval1(t1))
    case _ => throw NoRuleApplies
  }
}
}

object Partial {
  abstract class Tm
trait Nat {
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
trait Bool {
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
    def otherwise: Tm => OTm
  }
  trait Eval1 extends TmDefault { _: TmV =>
    type OTm = Tm
    def otherwise = _ => throw NoRuleApplies
  }
  val eval1: Eval1
}
trait Nat extends Term {
  type TmV <: TmVisit
  case object TmZero extends Tm {
    def accept(v: TmV): v.OTm = v.tmZero
  }
  case class TmSucc(t: Tm) extends Tm {
    def accept(v: TmV): v.OTm = v.tmSucc(t)
  }
  case class TmPred(t: Tm) extends Tm {
    def accept(v: TmV): v.OTm = v.tmPred(t)
  }
  trait TmVisit extends super.TmVisit { _: TmV =>
    def tmZero: OTm
    def tmSucc: Tm => OTm
    def tmPred: Tm => OTm
  }
  trait TmDefault extends TmVisit with super.TmDefault { _: TmV =>
    def tmZero = otherwise(TmZero)
    def tmSucc = t => otherwise(TmSucc(t))
    def tmPred = t => otherwise(TmPred(t))
  }
  def nv(t: Tm): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => nv(t1)
    case _ => false
  }
  trait Eval1 extends TmDefault with super.Eval1 { _: TmV =>
    override def tmSucc = t => TmSucc(this(t))
    override def tmPred = {
      case TmZero => TmZero
      case TmSucc(t) if nv(t) => t
      case t => TmPred(this(t))
    }
  }
}
object Nat extends Nat {
  type TmV = TmVisit
  object eval1 extends Eval1
}
}
