package fp.datastuctures.state

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
   * 10000000000000000000000000
   * 01111111111111111111111111
   * ↑のANDを取れば良いはず
   *
   * public static final int   MIN_VALUE = 0x80000000;
   * public static final int   MAX_VALUE = 0x7fffffff;
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) =
  (rng.nextInt._1 & Int.MaxValue, rng.nextInt._2)


  def double(rng: RNG): (Int, RNG) = ???

  def intDouble(rng: RNG): ((Int, Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???

  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???


}

