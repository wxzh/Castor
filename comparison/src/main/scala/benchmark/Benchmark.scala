package benchmark

trait Benchmark[A] {
  def benchmarkParsing(i: String): A

  def benchmarkEval(e: A): A

  def benchmark(i: String): Unit
}