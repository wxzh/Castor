package tapl.arith

import Arith._

object ArithDemo extends util.Demo[Unit, Command] {

  import util.Print._

  val width = 60

  override val initialContext: Unit = ()
  override val defaultExample: String = "examples/arith.tapl"

  override def parseInput(s: String): List[Command] =
    ArithParsers.input(s)

  override def processCommand(ctx: Unit, cmd: Command): Unit = cmd match {
    case Eval(t1) =>
      val doc1 = g2(ptmTerm(t1)(true, Context()) :: ";")
      val t2 = eval(Context(),t1)
      val doc2 = g2(ptmTerm(t2)(true, Context()) :: ";")

      println("====================")
      println(print(doc1, width))
      println("""||""")
      println("""\/""")
      println(print(doc2, width))
  }

}
