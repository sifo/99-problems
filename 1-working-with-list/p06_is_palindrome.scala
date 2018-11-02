
object Main {

  def main(args: Array[String]) {
    println(isPalindrome(List("x", "a", "m", "a", "x")))
    println(isPalindrome(List("a", "b")))
  }

  def rev[T](l: List[T]): List[T] = {
    l match {
      case Nil => Nil
      case hd :: rest => rev(rest) :+ hd
    }
  }

  def isPalindrome[T](l: List[T]): Boolean = {
    rev(l) == l
  }
}
