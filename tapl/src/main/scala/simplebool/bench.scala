package tapl.simplebool

import SimpleBool._

object Bench extends util.Bench[Tm] {
  def eval(t: Tm) = SimpleBool.eval(Context(),t)
  def parse(s: String) = SimpleBoolParsers.inputTm(s)(Context())
}