import scala.annotation.tailrec

object Main {

  sealed abstract class Node[T]
  case class One[T](e: T) extends Node[T]
  case class Many[T](i: Int, e: T) extends Node[T]
  object Many {
    def apply[T](i: Int, e: T) = {
      require(i >= 2)
      new Many(i, e)
    }
  }

  def main(args: Array[String]) {
    val l = List(Many(4, "a"), One("b"), Many(2, "c"), Many(2, "a"), One("b"), Many(4, "e"))
    println(decode(l))
    println(decode2(l))
  }

  def decode[T](l: List[Node[T]]): List[T] = {
    l match {
      case Nil => Nil
      case hd :: tail =>
        hd match {
          case One(e) => e :: decode(tail)
          case Many(i, e) =>
            if(i > 2)
              e :: decode(Many(i-1, e) :: tail)
            else
              e :: e :: decode(tail)
        }
    }
  }

  def decode2[T](list: List[Node[T]]): List[T] = {
    @tailrec
    def aux(l: List[Node[T]], acc: List[T]): List[T] = {
      l match {
        case Nil => acc
        case hd :: tail => {
          hd match {
            case One(e) => aux(tail, e :: acc)
            case Many(i, e) => {
              if(i > 2)
                aux(Many(i-1, e) :: tail, e :: acc)
              else
                aux(tail, e :: e :: acc)
            }
          }
        }
      }
    }
    aux(list, List()).reverse
  }
}
