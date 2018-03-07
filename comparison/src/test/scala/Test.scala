import benchmark.Benchmark
import tapl._
import util.Demo
import org.scalatest._
import org.scalameter._
import scala.io.Source

class Test extends FunSuite {

  def readLines(name: String): List[String] = {
    val inputFile = "examples/evaluation/" + name + ".txt"
    Source.fromFile(inputFile).getLines().toList
  }

  def benchmark[A](inputs: List[A], process: A => Unit, rep: Int): Quantity[Double] =
    config(
      Key.exec.benchRuns -> 10
      //Key.verbose -> true
    ) withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.IgnoringGC
    } measure {
      1 to rep foreach { _ => inputs.foreach(process) }
    }

  def output(name: String, t1: Double, t2: Double): Unit = println(f"$name & $t1%.1f & $t2%.1f")

  def compare[A,B](name: String, modular: util.Bench[A], nonmod: Benchmark[B]): Unit = {
    val lines: List[String] = readLines(name)

    val es1 = lines.map(modular.parse)
    val es2 = lines.map(nonmod.benchmarkParsing)

    val rep = 1
    val pe1 = benchmark(es1, modular.eval, rep).value
    val pe2 = benchmark(es2, nonmod.benchmarkEval, rep).value

    output(name, pe1, pe2)
  }

  def compareArith[A,B](name: String, modular: util.Bench[A], nonmod: util.Bench[B]): Unit = {
    val lines: List[String] = readLines(name)

    val es1 = lines.map(modular.parse)
    val es2 = lines.map(nonmod.parse)

    val rep = 1
    val pe1 = benchmark(es1, modular.eval, rep).value
    val pe2 = benchmark(es2, nonmod.eval, rep).value

    output(name, pe1, pe2)
  }

    test("Castor with monolithic") {
      compareArith("arith", comp.tapl.arith.Bench, comp.tapl.arith2.Bench)
    }

  test("Castor with monolithic") {
    compareArith("arith", tapl.arith.Bench, comp.tapl.arith.Bench)
  }

  test("EADD with partial function") {
    compareArith("arith", arith3.Bench, arith4.Bench)
  }

//  test("arith") {
//    compare("arith", arith.Bench, comp.tapl.arith.Demo)
//  }
//
//  test("untyped") {
//    compare("untyped", untyped.Bench, comp.tapl.untyped.Demo)
//  }
//
//  test("fulluntyped") {
//    compare("fulluntyped", fulluntyped.Bench, comp.tapl.fulluntyped.Demo)
//  }
//
//  test("tyarith") {
//    compare("tyarith", tyarith.Bench, comp.tapl.tyarith.Demo)
//  }
//
//  test("simplebool") {
//    compare("simplebool", simplebool.Bench, comp.tapl.simplebool.Demo)
//  }
//
//  test("fullsimple") {
//    compare("fullsimple", fullsimple.Bench, comp.tapl.fullsimple.Demo)
//  }
//
//  test("bot") {
//    compare("bot", bot.Bench, comp.tapl.bot.Demo)
//  }
//
//  test("fullerror") {
//    compare("fullerror", fullerror.Bench, comp.tapl.fullerror.Demo)
//  }
//
//  test("rcdsubbot") {
//    compare("rcdsubbot", rcdsubbot.Bench, comp.tapl.rcdsubbot.Demo)
//  }
//
//  test("fullsub") {
//    compare("fullsub", fullsub.Bench, comp.tapl.fullsub.Demo)
//  }
}
