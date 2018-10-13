package arith2

trait Term {
  trait Tm
  case class NoRuleApplies() extends Exception

  def eval1(t: Tm): Tm
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

  def eval1(t: Tm): Tm = t match {
    case TmSucc(t1) => TmSucc(eval1(t1))
    case TmPred(TmZero) => TmZero
    case TmPred(TmSucc(t1)) if isNumericVal(t1) => t1
    case TmPred(t1) => TmPred(eval1(t1))
    case _ => throw NoRuleApplies()
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
  def eval1(t: Tm): Tm = t match {
    case TmIf(TmTrue, t2, _) => t2
    case TmIf(TmFalse, _, t3) => t3
    case TmIf(t1,t2,t3) => TmIf(eval1(t1),t2,t3)
    case _ => throw NoRuleApplies()
  }
}

trait Arith extends Nat with Bool {
  case class TmIsZero(t: Tm) extends Tm

  override def eval1(t: Tm): Tm = t match {
    case _: TmSucc => super[Nat].eval1(t)
    case _: TmPred => super[Nat].eval1(t)
    case _: TmIf => super[Bool].eval1(t)
    case TmIsZero(TmZero) => TmTrue
    case TmIsZero(TmSucc(t1)) if isNumericVal(t1) => TmFalse
    case TmIsZero(t1) => TmIsZero(eval1(t1))
    case _ => throw NoRuleApplies()
  }

  override def isVal(t: Tm): Boolean = isNumericVal(t) || super[Bool].isVal(t)
}

object ArithImpl extends Arith

