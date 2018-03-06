package tapl.fulluntyped

import FullUntyped._

object Bench extends util.Bench[Tm] {
  def eval(t: Tm) = FullUntyped.eval(Context(),t)
  def parse(s: String) = FullUntypedParsers.inputTm(s)(Context())
}