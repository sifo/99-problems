
object Main {

  def main(args: Array[String]) {
    val l = List("a", "b", "c", "d")
    println(extractAt(0, l))
    println(extractAt(1, l))
    println(extractAt(3, l))
    println(extractAt(6, l))

    val m = List("a", "b", "c", "d", "e", "f", "g", "h")
    println(randSelect(m, 3))
  }

  def extractAt[T](index: Int, list: List[T]): (List[T], Option[T]) = {
    def aux(l: List[T], c: Int, acc: List[T]): (List[T], Option[T]) = {
      l match {
        case Nil => (acc.reverse, None)
        case hd :: tail =>
          if(c == 0)
            (acc.reverse ::: tail, Some(hd))
          else
            aux(tail, c-1, hd :: acc)
      }
    }
    aux(list, index, List())
  }

  def randSelect[T](l: List[T], i: Int): List[T] = {
    val r = (new scala.util.Random).nextInt(l.length)
    if(i > 0)
      extractAt(r, l) match {
        case (_, None) => List()
        case (rest, Some(e)) => e :: randSelect(rest, i-1)
      }
    else
      List()
  }
}
