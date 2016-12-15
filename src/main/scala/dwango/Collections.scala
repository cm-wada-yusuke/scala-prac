package dwango

object Collections {

  def swapArray[T](arr: Array[T])(i: Int, j: Int): Unit = {
    val temp = arr(i)
    arr(i) = arr(j)
    arr(j) = temp
    println(arr.toList)
  }

  def joinComma(start: Int, end: Int): String = start to end mkString ","

  def reverse[T](list: List[T]): List[T] =
    list.foldLeft(List.empty[T])((b: List[T], t: T) => t :: b)

  def sum(list: List[Int]): Int = list.foldRight(0)(_ + _)

  def mul(list: List[Int]): Int = list match {
    case Nil => 1
    case _ => list.foldRight(1)(_ * _)
  }

  def mkString[T](list: List[T])(sep: String): String =
    list.foldLeft("")((acc, t) => acc + sep + t.toString) match {
      case "" => ""
      case s => s.substring(1)
    }

  def main(args: Array[String]): Unit = {
    val arr = Array(1, 2, 3, 4, 5, 6)
    swapArray(arr)(1, 5)

    println(1 :: 2 :: Nil)

    println(joinComma(0, 32))

    val list = List(1, 2, 3, 4, 5, 6)

    println(reverse(list))

    println(sum(list))

    println(mul(List.empty[Int]))
    println(mul(List(1, 2, 3, 4, 556)))

    println(mkString(list)(";"))

    // 要素あり
    println(mkString(List("", "", "", ""))(";"))
    println(List("", "", "").mkString(","))

    //空
    println(mkString(List())(";"))
    println(List().mkString(","))

  }

}
