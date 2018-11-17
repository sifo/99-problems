
object Main {

  def main(args: Array[String]) {
    val l = List("a", "b", "c", "d", "e", "f", "g", "h")
    println(rotate(l, 3))
    println(rotate(l, -2))
  }

  def rotate[T](list: List[T], n: Int): List[T] = {
    if(n < 0) {
      list.reverse match {
        case Nil => Nil
        case hd :: tail =>
          rotate(hd :: tail.reverse, n+1)
      }
    } else if (n > 0) {
      list match {
        case Nil => Nil
        case hd :: tail =>
          rotate(tail :+ hd, n-1)
      }
    } else {
      list
    }
  }
}
