package fp.datastuctures

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG: SimpleRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

class NumberRNG extends RNG {

  override def nextInt: (Int, RNG) = ???

  def nonNegativeInt(rng: RNG): (Int, RNG) = ???

  def double(rng: RNG): (Int, RNG) = ???

  def intDouble(rng: RNG): ((Int, Double), RNG) = ???

  def doubleInt(rng: RNG): ((Double, Int), RNG) = ???

  def double3(rng: RNG): ((Double, Double, Double), RNG) = ???
}



