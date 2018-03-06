package comp.tapl.simplebool

import scala.io.Source

object Demo extends benchmark.Benchmark[Term] {

  import Evaluator._
  import PrettyPrinter._
  import comp.util.Print._

  val width = 60

  val name = "simplebool"

  def main(args: Array[String]): Unit = {
    val inputFile = "examples/" + name + ".txt"
    val lines: List[String] = Source.fromFile(inputFile).getLines().toList
    lines.foreach(process)
  }

  def process(i: String): Unit = {
    println(i)
    val e: Term = Parser.input(i)(Context())
    val t: Ty = Typer.typeof(Context(), e)
    val doc1 = g2(ptmATerm(true, Context(), e))
    println(print(doc1, width))
    println(s"Type: ${print(ptyTy(t))}")
    val t2 = eval(Context(), e)
    val doc2 = g2(ptmATerm(true, Context(), t2))
    println(print(doc2, width))
    println("-" * 80)
  }

  // process without output
  def benchmark(i: String): Unit = {
    val e = Parser.input(i)(Context())
    val t = Typer.typeof(Context(), e)
    val _ = eval(Context(), e)
  }

  override def benchmarkParsing(i: String): Term = Parser.input(i)(Context())

  override def benchmarkEval(e: Term): Term = {
    //val t = Typer.typeof(Context(), e)
    eval(Context(), e)
  }

}
