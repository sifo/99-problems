
object Main {

  sealed abstract class Node[T]
  case class One[T](e: T) extends Node[T]
  case class Many[T](i: Int, e: T) extends Node[T]

  def main(args: Array[String]) {
    val l = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e" ,"e")
    println(decode(encode(l)))
  }

  def encode[T](lst: List[T]): List[(Int,T)] = {
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

  def decode[T](l: List[(Int, T)]): List[Node[T]] = {
    l match {
      case Nil => Nil
      case hd :: tail =>
        if(hd._1 == 1)
          One(hd._2) :: decode(tail)
        else
          Many(hd._1, hd._2) :: decode(tail)
    }
  }
}
