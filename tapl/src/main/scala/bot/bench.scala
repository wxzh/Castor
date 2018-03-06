package tapl.bot

import Bot._

object Bench extends util.Bench[Tm] {
  def eval(t: Tm) = Bot.eval(Context(),t)
  def parse(s: String) = BotParsers.inputTm(s)(Context())
}