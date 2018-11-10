
object Main {

  sealed abstract class Node[T]
  case class One[T](e: T) extends Node[T]
  case class Many[T](i: Int, e: T) extends Node[T]

  def main(args: Array[String]) {
    val l = List(Many(4, "a"), One("b"), Many(2, "c"), Many(2, "a"), One("b"), Many(4, "e"))
    println(decode(l))
  }

  def decode[T](l: List[Node[T]]): List[T] = {
    l match {
      case Nil => Nil
      case hd :: tail =>
        hd match {
          case One(e) => e :: decode(tail)
          case Many(i, e) => if(i > 2) e :: decode(Many(i-1, e) :: tail) else e :: e :: decode(tail)
        }
    }
  }
}
