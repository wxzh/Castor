package arith4

import ArithImpl._

object Bench extends util.Bench[Tm] {
  def eval(t: Tm) = ArithImpl.eval(t)
  def parse(s: String) = ArithParsers.inputTm(s)
}