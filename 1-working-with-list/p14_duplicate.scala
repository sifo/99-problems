
object Main {

  def main(args: Array[String]) {
    val l = List("a", "b", "c", "c", "d")
    println(duplicate(l))
  }

  def duplicate[T](l: List[T]): List[T] = {
    l match {
      case Nil => Nil
      case hd :: tl => hd :: hd :: duplicate(tl)
    }
  }
}
