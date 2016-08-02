package fp.datastuctures.state

import scala.collection.immutable
import scala.collection.immutable._

//import fp.datastuctures.list
//import fp.datastuctures.list._


trait RNG {
  def nextInt: (Int, RNG)
}

object RNGOps {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG: SimpleRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  /**
    * 1000000000000...00000000000 ←最小値
    * 0111111111111...11111111111 ←最大値
    * ↑のANDを取れば良いはず
    *
    * public static final int   MIN_VALUE = 0x80000000;
    * public static final int   MAX_VALUE = 0x7fffffff;
    */
  def nonNegativeInt(rng: RNG): (Int, RNG) =
  (rng.nextInt._1 & Int.MaxValue, rng.nextInt._2)


  def double(rng: RNG): (Double, RNG) =
    (nonNegativeInt(rng)._1.toDouble / Int.MaxValue.toDouble, rng.nextInt._2)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (i, rng3) = nonNegativeInt(rng2)
    ((d, i), rng3)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (xs, last) = (0 until count).foldLeft((List.empty[Int], rng)) { (b, i) =>
      val (list, last) = b
      val (nextInt, nextRNG) = last.nextInt
      (nextInt :: list, nextRNG)
    }
    (xs.reverse, last)
  }
}

