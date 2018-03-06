package tapl.tyarith

import TyArith._

object Bench extends util.Bench[Tm] {
  def eval(t: Tm) = TyArith.eval(Context(),t)
  def parse(s: String) = ArithParsers.inputTm(s)
}