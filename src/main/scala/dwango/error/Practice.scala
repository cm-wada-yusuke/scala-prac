package dwango.error

object Practice {

  def main(args: Array[String]): Unit = {
    println(some2310)
    println(some2310FlatMap)
    println(some2310FlatMapFor)
  }

  private def some2310: Option[Int] = {
    val two = Some(2)
    val three = Some(3)
    val five = Some(5)
    val seven = Some(7)
    val eleven = Some(11)

    val first = two.map(i1 => three.map(i2 => i1 * i2)).flatten
    val second = first.map(i1 => five.map(i2 => i1 * i2)).flatten
    val third = second.map(i1 => seven.map(i2 => i1 * i2)).flatten
    val forth = third.map(i1 => eleven.map(i2 => i1 * i2)).flatten

    forth
  }

  private def some2310FlatMap: Option[Int] = {
    val two = Some(2)
    val three = Some(3)
    val five = Some(5)
    val seven = Some(7)
    val eleven = Some(11)

    two.flatMap(v1 => three.flatMap(v2 => five.flatMap(v3 => seven.flatMap(v4 => eleven.map(v5 => v1 * v2 * v3 * v4 * v5)))))
  }

  private def some2310FlatMapFor: Option[Int] = for {
    two <- Some(2)
    three <- Some(3)
    five <- Some(5)
    seven <- Some(7)
    eleven <- Some(11)
  } yield two * three * five * seven * eleven


}
