package util

import scala.util.Random

object Util {
  def random[T](s: Set[T]): T = {
    val n = Random.nextInt(s.size)
    s.iterator.drop(n).next
  }
}
