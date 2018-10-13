package arith4

trait Term {
  trait Tm
  case class NoRuleApplies() extends Exception

  def eval1: PartialFunction[Tm,Tm]
  def eval(t: Tm): Tm =
    try {
      val t1 = eval1(t)
      eval(t1)
    } catch {
      case _: NoRuleApplies => t
    }
}

trait Nat extends Term {
  case object TmZero extends Tm
  case class TmSucc(t: Tm) extends Tm
  case class TmPred(t: Tm) extends Tm

  def isNumericVal(t: Tm): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => isNumericVal(t1)
    case _ => false
  }

  def eval1: PartialFunction[Tm,Tm] = {
    case TmZero => throw NoRuleApplies()
    case TmSucc(t1) => TmSucc(eval1(t1))
    case TmPred(TmZero) => TmZero
    case TmPred(TmSucc(t1)) if isNumericVal(t1) => t1
    case TmPred(t1) => TmPred(eval1(t1))
  }
}

trait Bool extends Term {
  case object TmTrue extends Tm
  case object TmFalse extends Tm
  case class TmIf(t1: Tm, t2: Tm, t3: Tm) extends Tm

  def isVal(t: Tm) = t match {
    case TmTrue => true
    case TmFalse => true
    case _ => false
  }
  def eval1: PartialFunction[Tm,Tm] = {
    case TmIf(TmTrue, t2, _) => t2
    case TmIf(TmFalse, _, t3) => t3
    case TmIf(t1,t2,t3) => TmIf(eval1(t1),t2,t3)
    case TmTrue => throw NoRuleApplies()
    case TmFalse => throw NoRuleApplies()
  }
}

trait Arith extends Nat with Bool {
  case class TmIsZero(t: Tm) extends Tm

  override def eval1 = super[Nat].eval1 orElse super[Bool].eval1 orElse {
    case TmIsZero(TmZero) => TmTrue
    case TmIsZero(TmSucc(t1)) if isNumericVal(t1) => TmFalse
    case TmIsZero(t1) => TmIsZero(eval1(t1))
  }

  override def isVal(t: Tm): Boolean = isNumericVal(t) || super[Bool].isVal(t)
}

object ArithImpl extends Arith

