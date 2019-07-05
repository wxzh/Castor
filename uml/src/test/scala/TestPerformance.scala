import uml.Original._

import org.scalatest._
import org.scalameter._

class TestPerformance extends FunSuite {
  def executeActivity(a: Activity): Unit = {
    a.main(a.inputs.map(v => new InputValue{ value=v.initialValue; variable=v}))
  }

  def benchmark[A](input:A, process: A => Unit, rep: Int = 1): Unit = {
    val res = config(
      Key.exec.benchRuns -> 10
      //Key.verbose -> true
    ) withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.IgnoringGC
    } measure {
      process(input)
    }
    println(f"time: ${res.value}%.1f")
  }


 test("variant1") {
   benchmark(Variant1.activity, executeActivity)
 }
 test("variant2") {
   benchmark(Variant2.activity, executeActivity)
 }
 test("variant3") {
   benchmark(Variant3.activity, executeActivity)
 }
}
