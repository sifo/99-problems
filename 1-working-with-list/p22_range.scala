
object Main {

  def main(args: Array[String]) {
    println(range(4, 9))
    println(range(9, 4))
  }

  def range(a: Int, b: Int): List[Int] = {
    if(a < b)
      a :: range(a+1, b)
    else if (b < a)
      a :: range(a-1, b)
    else
      List(a)
  }
}
