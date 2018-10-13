package arith5

object ArithImpl {
  trait Tm
  case object TmZero extends Tm
  case class TmSucc(t: Tm) extends Tm
  case class TmPred(t: Tm) extends Tm
  case object TmTrue extends Tm
  case object TmFalse extends Tm
  case class TmIf(t1: Tm, t2: Tm, t3: Tm) extends Tm
  case class TmIsZero(t: Tm) extends Tm

  def eval1(t: Tm): Tm = t match {

    case TmIf(TmTrue, t2, t3) =>
      t2
    case TmIf(TmFalse, t2, t3) =>
      t3
    case TmIf(t1, t2, t3) =>
      val t11 = eval1(t1)
      TmIf(t11, t2, t3)
    case TmSucc(t1) =>
      val t11 = eval1(t1)
      TmSucc(t11)
    case TmPred(TmZero) =>
      TmZero
    case TmPred(TmSucc(nv1)) if isNumericVal(nv1) =>
      nv1
    case TmPred(t1) =>
      val t2 = eval1(t1)
      TmPred(t2)
    case TmIsZero(TmZero) =>
      TmTrue
    case TmIsZero(TmSucc(nv1)) if isNumericVal(nv1) =>
      TmFalse
    case TmIsZero(t1) =>
      val t2 = eval1(t1)
      TmIsZero(t2)
    case _ => throw new NoRuleApplies
  }
//    case TmSucc(t1) => TmSucc(eval1(t1))
//    case TmPred(TmZero) => TmZero
//    case TmPred(TmSucc(t1)) if isNumericVal(t1) => t1
//    case TmPred(t1) => TmPred(eval1(t1))
//    case TmIf(TmTrue, t2, _) => t2
//    case TmIf(TmFalse, _, t3) => t3
//    case TmIf(t1,t2,t3) => TmIf(eval1(t1),t2,t3)
//    case TmIsZero(TmZero) => TmTrue
//    case TmIsZero(TmSucc(t1)) if isNumericVal(t1) => TmFalse
//    case TmIsZero(t1) => TmIsZero(eval1(t1))
//    case _ => throw NoRuleApplies()
//  }
  case class NoRuleApplies() extends Exception

  def eval(t: Tm): Tm =
    try {
      val t1 = eval1(t)
      eval(t1)
    } catch {
      case _: NoRuleApplies => t
    }

  def isNumericVal(t: Tm): Boolean = t match {
    case TmZero => true
    case TmSucc(t1) => isNumericVal(t1)
    case _ => false
  }

  def isVal(t: Tm) = isNumericVal(t) || (t match {
    case TmTrue => true
    case TmFalse => true
    case _ => false
  })
}

