import scala.annotation.tailrec

object Main {

  def main(args: Array[String]) {
    val l = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
    println(split(l, 3))
    println(split(l, 0))
    println(split(l, -1))
    val m = List("a", "b", "c", "d")
    println(split(m, 5))
  }

  def split[T](list: List[T], n: Int): List[List[T]] = {
    @tailrec
    def aux(l: List[T], c: Int, acc: List[T]): List[List[T]] = {
      l match {
        case Nil => List(acc.reverse, List())
        case hd :: tail =>
          if(c <= 0)
            List(acc.reverse, l)
          else
            aux(tail, c-1, hd :: acc)
      }
    }
    aux(list, n, List())
  }
}
