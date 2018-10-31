
object Main {

  def main(args: Array[String]) {
    println(length(List("a", "b", "c")))
  }

  def length[T](l: List[T]): Int = {
    l match {
      case Nil => 0
      case hd :: rest => 1 + length(rest)
    }
  }
}
