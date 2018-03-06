package tapl.untyped

import Untyped._

object Bench extends util.Bench[Tm] {
  def eval(t: Tm) = Untyped.eval(Context(),t)
  def parse(s: String) = UntypedParsers.inputTm(s)(Context())
}