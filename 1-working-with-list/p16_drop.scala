import scala.annotation.tailrec

object Main {

  def main(args: Array[String]) {
    val l = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
    println(dropEveryNth(l, 3))
  }

  def dropEveryNth[T](list: List[T], n: Int): List[T] = {
    @tailrec
    def aux(l: List[T], c: Int, acc: List[T]): List[T] = {
      l match {
        case Nil => acc
        case hd :: tail =>
          if(c <= 1)
            aux(tail, n, acc)
          else
            aux(tail, c-1, hd :: acc)
      }
    }
    aux(list, n, List()).reverse
  }
}
