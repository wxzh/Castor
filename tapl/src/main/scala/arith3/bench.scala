package arith3

object Bench extends util.Bench[Tm] {
  def eval(t: Tm) =
    try {
      val t1 = t.accept(eval1)
      eval(t1)
    } catch {
      case _: NoRuleApplies => t
    }
  def parse(s: String) = ArithParsers.inputTm(s)
}