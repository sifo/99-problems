import scala.annotation.tailrec

object Main {

  def main(args: Array[String]) {
    val l = List("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
    println(slice(l, 2, 6))
  }

  def slice[T](list: List[T], i: Int, k: Int): List[T] = {
    @tailrec
    def aux(l: List[T], c: Int, acc: List[T]): List[T] = {
      l match {
        case Nil => acc
        case hd :: tail =>
          if(c < i)
            aux(tail, c+1, acc)
          else if(c >= i && c <= k)
            aux(tail, c+1, hd :: acc)
          else
            acc
      }
    }
    aux(list, 0, List()).reverse
  }
}
