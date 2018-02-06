package tapl.tyarith

import TyArith._

object TyArithDemo extends util.Demo[Unit, Command] {

  import util.Print._

  val width = 60

  override val initialContext: Unit = ()
  override val defaultExample: String = "examples/tyarith.tapl"

  override def parseInput(s: String): List[Command] =
    ArithParsers.input(s)

  def processCommand(ctx: Unit, cmd: Command): Unit = cmd match {
    case Eval(t1) =>
      val ty1 = typeof(t1)(Context())
      val doc1 = g2(ptm(t1,Context()) :: ":" :/: ptyTy(ty1,Context()) :: ";")

      val t2 = eval(Context(),t1)
      val ty2 = typeof(t2)(Context())
      val doc2 = g2(ptm(t2,Context()) :: ":" :/: ptyTy(ty2,Context()) :: ";")

      println("====================")
      println(print(doc1, width))
      println("""||""")
      println("""\/""")
      println(print(doc2, width))

  }

}
