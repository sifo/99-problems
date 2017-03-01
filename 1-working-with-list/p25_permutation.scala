import scala.util.Random;

object App {

  def at[T](i: Int, l: List[T]): Option[T] = {
    l match {
      case Nil => None
      case h :: t => if(i == 0) Some(h) else at(i-1, t)
    }
  }

  def removeAt[T](i: Int, lst: List[T]): List[T] = {
    def aux[T](acc: List[T], cur: Int, l: List[T]): List[T] = {
      l match {
        case Nil => acc
        case h :: t => if(cur == 0) acc ::: t else aux(h::acc, cur-1, t)
      }
    }
    aux(List(), i, lst)
  }

  def permutation[T](lst: List[T]): List[T] = {
    def aux[T](acc: List[T], l: List[T]): List[T] = {
      l match {
        case Nil => acc
        case _ =>
          val r = Random.nextInt(l.size)
          val e = at(r, l) match { case Some(x) => x; case None => throw new Exception() }
          val sl = removeAt(r, l)
          aux(e::acc, sl)
      }
    }
    aux(List(), lst)
  }

  def main(args: Array[String]) {
    val l = List("a", "b", "c", "d", "e" ,"f")
    println(l)
    println(permutation(l))
  }
}
