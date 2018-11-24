
object Main {

  def main(args: Array[String]) {
    println(range(4, 9))
    println(range(9, 4))
    println(range2(4, 9))
    println(range2(9, 4))
  }

  def range(a: Int, b: Int): List[Int] = {
    if(a < b)
      a :: range(a+1, b)
    else if (b < a)
      a :: range(a-1, b)
    else
      List(a)
  }

  def range2(a: Int, b: Int): List[Int] = {
    def aux(cur: Int, acc: List[Int]): List[Int] = {
      if(cur < b)
        aux(cur+1, cur :: acc)
      else if(b < cur)
        aux(cur-1, cur :: acc)
      else
        (cur :: acc).reverse
    }
    aux(a, List())
  }
}
