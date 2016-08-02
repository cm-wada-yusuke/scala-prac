package fp.datastuctures.state

import fp.datastuctures.state.RNGOps.SimpleRNG

object StateOut {

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(20160802)

    println("nonNegativeInt:")
    l(5)(rng)(RNGOps.nonNegativeInt)

    println("double:")
    l(5)(rng)(RNGOps.double)

    println("intDouble:")
    l(5)(rng)(RNGOps.intDouble)

    println("doubleInt:")
    l(5)(rng)(RNGOps.doubleInt)

    println("double3:")
    l(5)(rng)(RNGOps.double3)

    println("ints:")
    println(RNGOps.ints(5)(rng))


  }

  private def l[A](n: Int)(next: RNG)(f: RNG => (A, RNG)): Unit = n match {
    case 0 => ()
    case x: Int => {
      val res = f(next)
      println(res)
      l(x - 1)(res._2)(f)
    }

  }


}
