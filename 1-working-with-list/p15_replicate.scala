import scala.annotation.tailrec

object Main {

  def main(args: Array[String]) {
    val l = List("a", "b", "c")
    println(replicate(l, 3))
  }

  def rep[T](e: T, count: Int): List[T] = {
    @tailrec
    def aux(cur: Int, acc: List[T]): List[T] = {
      if(cur > 0)
        aux(cur-1, e :: acc)
      else
        acc
    }
    aux(count, List())
  }

  def replicate[T](list: List[T], count: Int): List[T] = {
    @tailrec
    def aux(l: List[T], acc: List[T]): List[T] = {
      l match {
        case Nil => acc
        case head :: tail => aux(tail, rep(head, count) ::: acc)
      }
    }
    aux(list, List())
  }
}
