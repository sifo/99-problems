object Main {

  def main(args: Array[String]) {
    println(lastTwo(List("a", "b", "c", "d")))
    println(lastTwo(List("a")))
  }

  def lastTwo[T](l: List[T]): Option[(T, T)] = l match {
    case Nil => None
    case a :: Nil => None
    case a :: b :: Nil => Some((a, b))
    case hd :: rest => lastTwo(rest)
  }
}
