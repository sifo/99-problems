
object Main {


  def main(args: Array[String]) {
    val l = List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e" ,"e")
    println(compress(l))
  }

  def compress[T](l: List[T]): List[T] = {
    l match {
      case Nil => Nil
      case a :: Nil => List(a)
      case a :: b :: tail => if(a == b) compress(b :: tail) else a :: compress(b :: tail)
    }
  }

}
