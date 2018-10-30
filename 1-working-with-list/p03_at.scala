
object Main {

  def main(args: Array[String]) {
    println(at(3, List("a", "b", "c", "d")))
  }

  def at[T](idx: Int, l: List[T]): Option[T] = {
    if(idx > l.length || idx <= 0)
      None
    else {
      l match {
        case Nil => None
        case hd :: rest => if(idx == 1) Some(hd) else at(idx-1, rest)
      }
    }
  }
}
