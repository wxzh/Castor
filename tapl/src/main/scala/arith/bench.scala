package tapl.arith

import Arith._

object Bench extends util.Bench[Tm] {
  def eval(t: Tm) = Arith.eval(Context(),t)
  def parse(s: String) = ArithParsers.inputTm(s)
}