object Main {

  def main(args: Array[String]) {
    val l = List("a", "b", "c", "d")
    println(insertAt("alpha", 1, l))
    println(insertAt("alpha", 0, l))
    println(insertAt("alpha", 3, l))
    println(insertAt("alpha", 4, l))
    println(insertAt("alpha", 6, l))
  }

  def insertAt[T](e: T, index: Int, list: List[T]): List[T] = {
    def aux(l: List[T], c: Int, acc: List[T]): List[T] = {
      l match {
        case Nil => (e :: acc).reverse
        case hd :: tail =>
          if(c == 0)
            acc.reverse ::: (e :: l)
          else
            aux(tail, c-1, hd :: acc)
      }
    }
    aux(list, index, List())
  }
}
