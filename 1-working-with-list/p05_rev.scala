
object Main {

  def main(args: Array[String]) {
    println(rev(List("a", "b", "c")))
  }

  def rev[T](l: List[T]): List[T] = {
    l match {
      case Nil => Nil
      case hd :: rest => rev(rest) :+ hd
    }
  }
}
