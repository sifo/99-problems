
object Main {

  def main(args: Array[String]) {
    println(lottoSelect(6, 49))
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

  def range(a: Int, b: Int): List[Int] = {
    def aux(cur: Int, acc: List[Int]): List[Int] = {
      if(cur < b)
        aux(cur+1, cur :: acc)
      else if(b < cur)
        aux(cur-1, cur :: acc)
      else
        (cur :: acc).reverse
    }
    aux(a, List())
  }

  def lottoSelect(n: Int, m: Int): List[Int] = {
    randSelect(range(1, m), n)
  }
}
