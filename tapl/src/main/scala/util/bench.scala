package util

trait Bench[T] {
  def eval(t: T): T
  def parse(s: String): T
}