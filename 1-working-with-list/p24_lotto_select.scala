
object Main {

  def main(args: Array[String]) {
    val l = List("a", "b", "c", "d", "e", "f", "g", "h")
    println(lottoSelect(l, 3))
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

  def lottoSelect[T](l: List[T], n: Int): List[T] = {
    val r = (new scala.util.Random).nextInt(l.length)
    if(n > 0)
      l(r) :: lottoSelect(extractAt(r, l)._1, n-1)
    else
      List()
  }
}
