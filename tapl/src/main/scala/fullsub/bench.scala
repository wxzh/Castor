package tapl.fullsub

import FullSub._

object Bench extends util.Bench[Tm] {
  def eval(t: Tm) = FullSub.eval(Context(),t)
  def parse(s: String) = FullSubParsers.inputTm(s)(Context())
}