package arith3

abstract class Tm {
  def accept[O](v: TmVisit[O]): O
}
object TmZero extends Tm {
  def accept[O](v: TmVisit[O]) = v.tmZero()
}
class TmSucc(t: Tm) extends Tm {
  def accept[O](v: TmVisit[O]) = v.tmSucc(t)
}
class TmPred(t: Tm) extends Tm {
  def accept[O](v: TmVisit[O]) = v.tmPred(t)
}
object TmTrue extends Tm {
  def accept[O](v: TmVisit[O]) = v.tmTrue
}
object TmFalse extends Tm {
  def accept[O](v: TmVisit[O]) = v.tmFalse
}
class TmIf(t1: Tm, t2: Tm, t3: Tm) extends Tm {
  def accept[O](v: TmVisit[O]) = v.tmIf(t1,t2,t3)
}
class TmIsZero(t: Tm) extends Tm {
  def accept[O](v: TmVisit[O]) = v.tmIsZero(t)
}

trait TmVisit[O] {
  def tmZero(): O
  def tmSucc(t: Tm): O
  def tmPred(t: Tm): O
  def tmTrue(): O
  def tmFalse(): O
  def tmIf(t1: Tm, t2: Tm, t3: Tm): O
  def tmIsZero(t: Tm): O
}
case class NoRuleApplies() extends Exception

object nv extends TmVisit[Boolean] {
  def tmZero() = true
  def tmSucc(t: Tm) = t.accept(nv)
  def tmPred(t: Tm) = false
  def tmTrue() = false
  def tmFalse() = false
  def tmIf(t1: Tm, t2: Tm, t3: Tm) = false
  def tmIsZero(t: Tm) = false
}

object eval1 extends TmVisit[Tm] {
  def tmZero() = throw NoRuleApplies()
  def tmSucc(t: Tm) =
    new TmSucc(t.accept(eval1))
  def tmPred(t: Tm) = t.accept(new TmVisit[Tm] {
    def tmZero() = TmZero
    def tmSucc(t1: Tm) = if (t1.accept(nv)) t1 else new TmSucc(t1.accept(eval1))
    def tmPred(t1: Tm) = new TmPred(t.accept(eval1))
    def tmTrue() = new TmPred(t.accept(eval1))
    def tmFalse() = new TmPred(t.accept(eval1))
    def tmIf(t1: Tm, t2: Tm, t3: Tm) = new TmPred(t.accept(eval1))
    def tmIsZero(t1: Tm) = new TmPred(t.accept(eval1))
  })
  def tmTrue() = throw NoRuleApplies()
  def tmFalse() = throw NoRuleApplies()
  def tmIf(t1: Tm, t2: Tm, t3: Tm) = t1.accept(new TmVisit[Tm] {
    def tmTrue() = t2
    def tmFalse() = t3
    def tmZero() = new TmIf(t1.accept(eval1), t2, t3)
    def tmSucc(t1: Tm) = new TmIf(t1.accept(eval1), t2, t3)
    def tmPred(t1: Tm) = new TmIf(t1.accept(eval1), t2, t3)
    def tmIf(s1: Tm, s2: Tm, s3: Tm) = new TmIf(t1.accept(eval1), t2, t3)
    def tmIsZero(s: Tm) = new TmIf(t1.accept(eval1), t2, t3)
  })

  def tmIsZero(t: Tm) = t.accept(new TmVisit[Tm] {
    def tmTrue() = TmTrue
    def tmSucc(t1: Tm) = if (t1.accept(nv)) TmFalse else new TmIsZero(t.accept(eval1))
    def tmZero () = new TmIsZero(t.accept(eval1))
    def tmPred(t1: Tm) = new TmIsZero(t.accept(eval1))
    def tmFalse() = new TmIsZero(t.accept(eval1))
    def tmIf(t1: Tm, t2: Tm, t3: Tm) = new TmIsZero(t.accept(eval1))
    def tmIsZero(t1: Tm) = new TmIsZero(t.accept(eval1))
  })
}
