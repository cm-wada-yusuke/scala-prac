package fp.datastuctures.state

import scala.collection.immutable._

//import fp.datastuctures.list
//import fp.datastuctures.list._

case class State[S, +A](run: S => (A, S)) {
  def get: State[S, S] = State(s => (s, s))

  def set(s: S): State[S, Unit] = State(_ => ((), s))

  def modify(f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def sequence(fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(State.unit(List.empty))(State.map2[A, List[A], List[A], S](_, _)(_ :: _))

}

object State {

  type State[S, +A] = S => (A, S)
  type Rand[A] = State[RNG, A]


  def unit[A, S](a: A): State[S, A] = state => (a, state)

  def map[A, B, S](s: State[S, A])(f: A => B): State[S, B] = state => {
    val (a, s2) = s(state)
    (f(a), s2)
  }

  def map2[A, B, C, S](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = state => {
    val (a, st2) = sa(state)
    val (b, st3) = sb(st2)
    (f(a, b), st3)
  }

  def flatMap[A, B, S](f: State[S, A])(g: A => State[S, B]): State[S, B] = { state =>
    val (b, st2) = map(f)(g)(state)
    b(st2)
  }

}


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
    (nonNegativeInt(rng)._1.toDouble / (Int.MaxValue.toDouble + 1), rng.nextInt._2)

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

  // FoldRightを使う
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (xs, last) = (0 until count).foldLeft((List.empty[Int], rng)) { (b, i) =>
      val (list, last) = b
      val (nextInt, nextRNG) = last.nextInt
      (list ::: List(nextInt), nextRNG)
    }
    (xs, last)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)


  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def randDouble: Rand[Double] =
    map(nonNegativeInt)(_.toDouble / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def randIntDouble: Rand[(Int, Double)] = both(int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /*
   * 2つのRNG遷移の組み合わせが可能であるとしたら、それらのリスト全体を結合することも可能であるはずだ。
   * 遷移のListを1つの遷移にまとめるためのsequenceを実装せよ。それを使って、以前に記述したints関数を再実装せよ。
   * その際には、標準ライブラリのList.fill(n)(x)関数を使ってxをn回繰り返すリストを生成できる。
   */
  // fsをfoldRightでRand[List[A]]に形を変えながら進む方針
  // (b, a) => b では、map2を使う。(List[A], A, List[A]) => Rand(List[A]) とする。
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  fs.foldRight(unit(List.empty[A]))(map2[A, List[A], List[A]](_, _)(_ :: _))

  // まず、List.fillでList[Rand[Int]]をつくる。
  // 次に、そのListに対してsequenceを適用する。そうするとRand[List[Int]]が出来上がるので、ドミノだおしスタートのrngを渡してやる。
  def sequenceInts(count: Int)(rng: RNG): (List[Int], RNG) =
  sequence(List.fill(count)(int))(rng)


  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (b, rng2) = map(f)(g)(rng)
    b(rng2)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i: Int =>
    rng => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) (mod, rng) else nonNegativeLessThan(n)(rng)
    }
  }

  def mapF[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s) { a => (f(a), _) }

  def map2F[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a => flatMap(rb) { b => (f(a, b), _) } }

}

