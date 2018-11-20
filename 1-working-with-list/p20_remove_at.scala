
object Main {

  def main(args: Array[String]) {
    val l = List("a", "b", "c", "d")
    println(removeAt(1, l))
    println(removeAt(0, l))
    println(removeAt(3, l))
    println(removeAt(6, l))
    println(removeAt2(1, l))
    println(removeAt2(0, l))
    println(removeAt2(3, l))
    println(removeAt2(6, l))
  }

  def removeAt[T](index: Int, list: List[T]): List[T] = {
    def aux(l: List[T], c: Int, acc: List[T]): List[T] = {
      l match {
        case Nil => acc.reverse
        case hd :: tail =>
          if(c == 0)
            acc.reverse ::: tail
          else
            aux(tail, c-1, hd :: acc)
      }
    }
    aux(list, index, List())
  }
  def removeAt2[T](index: Int, list: List[T]): List[T] = {
    list match {
      case Nil => list
      case hd :: tail =>
        if(index == 0)
          tail
        else
          hd :: removeAt2(index-1, tail)
    }
  }
}
