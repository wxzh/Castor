package tapl.bot

import Bot._

object BotDemo extends util.Demo[Context, Command] {
  import util.Print._

  val width = 60

  override val initialContext: Context = Context()
  override val defaultExample: String = "examples/bot.tapl"

  override def parseInput(s: String): List[Command] =
    BotParsers.input(s)(Context())._1

  def processCommand(ctx: Context, cmd: Command): Context = cmd match {
    case Eval(t1) =>
      val ty1 = typeof(t1)(ctx)
      val doc1 = g2(ptmATerm(t1)(true, ctx) :: ":" :/: ptyType(ty1)(true,ctx) :: ";")

      val t2 = eval(ctx,t1)
      val ty2 = typeof(t2)(ctx)
      val doc2 = g2(ptmATerm(t2)(true, ctx) :: ":" :/: ptyType(ty2)(true,ctx) :: ";")

      println("====================")
      println(print(doc1, width))
      println("""||""")
      println("""\/""")
      println(print(doc2, width))

      ctx

    case Bind(x, bind) =>
      val doc1 = x :: pBinding(bind)(ctx) :: ";"
      println("====================")
      println(print(doc1, width))
      ctx.addBinding(x, bind)
  }

}