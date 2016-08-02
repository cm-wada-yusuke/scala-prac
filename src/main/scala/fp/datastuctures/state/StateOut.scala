package fp.datastuctures.state

import fp.datastuctures.state.RNGOps.SimpleRNG

object StateOut {

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(20160802)
    print("nonNegativeInt: ")
    Seq(0 until 20).foreach { _ =>
      println(RNGOps.nonNegativeInt(rng))
    }

  }

}
