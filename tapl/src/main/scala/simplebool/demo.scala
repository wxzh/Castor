package tapl.simplebool

import SimpleBool._

object SimpleBoolDemo extends util.Demo[Context, Command] {
  import util.Print._

  val width = 60

  override val initialContext: Context = Context()
  override val defaultExample: String = "examples/simplebool.tapl"

  override def parseInput(s: String): List[Command] =
    SimpleBoolParsers.input(s)(Context())._1

  def processCommand(ctx: Context, cmd: Command): Context = cmd match {
    case Eval(t1) =>

      val ty1 = typeof(t1)(ctx)
      val doc1 = g2(ptmATerm(t1)(true, ctx) :: ":" :/: ptyTy(ty1,ctx) :: ";")

      val t2 = eval(ctx,t1)
      val ty2 = typeof(t2)(ctx)
      val doc2 = g2(ptmATerm(t2)(true, ctx) :: ":" :/: ptyTy(ty2,ctx) :: ";")

      println("====================")
      println(print(doc1, width))
      println("""||""")
      println("""\/""")
      println(print(doc2, width))

      ctx
    case Bind(n, b) =>
      val doc1 = n :: pBinding(b)(ctx) :: ";"

      println("====================")
      println(print(doc1, width))

      ctx.addBinding(n, b)
  }

}