
object Main {

  def main(args: Array[String]) {
    val l = List("a", "b", "c", "d", "e", "f", "g", "h")
    println(rotate(l, 3))
    println(rotate(l, -2))
    println(rotate2(l, 3))
    println(rotate2(l, -2))
  }

  def rotate[T](list: List[T], n: Int): List[T] = {
    if(n < 0) {
      list.reverse match {
        case Nil => Nil
        case hd :: tail =>
          rotate(hd :: tail.reverse, n+1)
      }
    } else if (n > 0) {
      list match {
        case Nil => Nil
        case hd :: tail =>
          rotate(tail :+ hd, n-1)
      }
    } else {
      list
    }
  }


  def split[T](list: List[T], n: Int): (List[T], List[T]) = {
    def aux(l: List[T], c: Int, acc: (List[T], List[T])): (List[T], List[T]) = {
      l match {
        case Nil => (acc._1.reverse, List())
        case hd :: tail =>
          if(c == 0)
            (acc._1.reverse, l)
          else
            aux(tail, c-1, (hd :: acc._1, tail))
      }
    }
    aux(list, n, (List(), List()))
  }

  def rotate2[T](list: List[T], n: Int): List[T] = {
    if(n == 0) {
      list
    } else {
      val m = (n % list.length + list.length) % list.length
      val r = split(list, m)
      r._2 ::: r._1
    }
  }
}
