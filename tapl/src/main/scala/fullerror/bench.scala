package tapl.fullerror

import FullError._

object Bench extends util.Bench[Tm] {
  def eval(t: Tm) = FullError.eval(Context(),t)
  def parse(s: String) = FullErrorParsers.inputTm(s)(Context())
}