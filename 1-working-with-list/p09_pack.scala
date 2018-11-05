
object Main {

  def main(args: Array[String]) {
    val l = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e" ,"e")
    println(pack(l))
  }

  def pack[T](lst: List[T]): List[List[T]] = {
    def aux[T](current: List[T], acc: List[List[T]], l: List[T]): List[List[T]] = {
      l match {
        case Nil => acc
        case hd :: Nil => (hd :: current) :: acc
        case a :: b :: t =>
          if(a == b)
            aux(a :: current, acc, b :: t)
          else
            aux(List(), (a :: current) :: acc, b :: t)
      }
    }
    aux(List(), List(), lst).reverse
  }
}
