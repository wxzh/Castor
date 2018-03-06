package tapl.fullsimple

import FullSimple._

object Bench extends util.Bench[Tm] {
  def eval(t: Tm) = FullSimple.eval(Context(),t)
  def parse(s: String) = FullSimpleParsers.inputTm(s)(Context())
}