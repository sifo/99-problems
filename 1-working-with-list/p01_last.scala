
object Main {

  def main(args: Array[String]) {
    println(last(List("a", "b", "c", "d")))
    println(last(List()))
  }

  def last[T](l: List[T]): Option[T] = l match {
    case Nil => None
    case hd :: Nil => Some(hd)
    case hd :: rest => last(rest)
  }
}
