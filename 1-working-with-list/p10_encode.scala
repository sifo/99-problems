

object Main {

  def main(args: Array[String]) {
    val l = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e" ,"e")
    println(encode(pack(l)))
    println(encode2(l))
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

  def encode[T](l: List[List[T]]): List[(Int, T)] = {
    l match {
      case Nil => Nil
      case hd :: tail => (hd.length, hd(0)) :: encode(tail)
    }
  }

  def encode2[T](lst: List[T]): List[(Int,T)] = {
    def aux[T](count: Int, acc: List[(Int, T)], l: List[T]): List[(Int, T)] = {
      l match {
        case Nil => acc
        case hd :: Nil => (count+1, hd) :: acc
        case a :: b :: t =>
          if(a == b)
            aux(count+1, acc, b :: t)
          else
            aux(0, (count+1, a) :: acc, b :: t)
      }
    }
    aux(0, List(), lst).reverse
  }
}
