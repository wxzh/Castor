package tapl.fulluntyped

import FullUntyped._

object FullUntypedDemo extends util.Demo[Context, Command] {
  import util.Print._

  val width = 60

  override val initialContext: Context = Context()
  override val defaultExample: String = "examples/fulluntyped.tapl"

  override def parseInput(s: String): List[Command] =
    FullUntypedParsers.input(s)(Context())._1

  def processCommand(ctx: Context, cmd: Command): Context = cmd match {
    case Eval(t1) =>
      val doc1 = g2(ptmATerm(t1)(true, ctx) :: ";")
      val t2 = eval(ctx,t1)
      val doc2 = g2(ptmATerm(t2)(true, ctx) :: ";")

      println("====================")
      println(print(doc1, width))
      println("""||""")
      println("""\/""")
      println(print(doc2, width))

      ctx

    case Bind(n, b) =>
      ctx.addBinding(n, b)
  }

}
