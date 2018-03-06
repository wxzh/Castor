package tapl.rcdsubbot

import RcdSubBot._

object Bench extends util.Bench[Tm] {
  def eval(t: Tm) = RcdSubBot.eval(Context(),t)
  def parse(s: String) = RcdSubBotParsers.inputTm(s)(Context())
}