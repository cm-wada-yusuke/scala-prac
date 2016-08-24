package fp.datastuctures.state


sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    inputs.foldLeft(State[Machine, (Int, Int)](m => (0, 0), Machine(true, 10, 5)) {
      (sm, input) => simulate(sm, input)
    }

  def simulate(machine: State[Machine, (Int, Int)])(input: Input): State[Machine, (Int, Int)] = input match {
    case Coin => for {
      mc: Machine <- machine.modify(m => Machine(false, m.candies, m.candies + 1)).get
    } yield (mc, (mc.coins, mc.candies))
    case Turn => for {
      mc: Machine <- machine.modify(m => Machine(true, m.candies - 1, m.coins)).get
    } yield (mc, (mc.coins, mc.candies))
  }
}


// State.flatMap[Unit, (Int, Int), Machine](machine.modify(m => Machine(false, m.candies, m.coins)))(_ => (coin + 1, snack))