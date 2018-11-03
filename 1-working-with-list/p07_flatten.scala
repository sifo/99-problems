
object Main {

  sealed abstract class Node[T]
  case class One[T](t: T) extends Node[T]
  case class Many[T](l: List[Node[T]]) extends Node[T]

  def main(args: Array[String]) {
    val l = List(One("a"), Many(List(One("b"), Many(List(One("c"), One("d"))), One("e"))))
    println(flatten(l))
  }

  def flatten[T](l: List[Node[T]]): List[T] = {
    l match {
      case Nil => Nil
      case hd :: rest =>
        hd match {
          case One(t) => t :: flatten(rest)
          case Many(t) => flatten(t) ::: flatten(rest)
        }
    }
  }

}
