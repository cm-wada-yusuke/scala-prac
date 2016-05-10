package fp.datastuctures

/**
 * Created by y-wada on 2016/03/21.
 */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  implicit val print: Boolean = true

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def str[A](as: List[A]): String =
    foldLeft(as, "[")((b: String, a: A) => b + a.toString) + "]"

  /**
   * EX32.
   * データ構造から考えて、2番目のリストの要素を返せば目的は達成できる。
   *
   * @param as
   * @tparam A
   * @return
   */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, y) => y
  }

  /**
   * EX33.
   * 問の趣旨(matchを使う)から少し外れるかもしれないが、
   * せっかくなので先のtail関数を使うことにする。
   *
   * @param head
   * @param as
   * @tparam A
   * @return
   */
  def setHead[A](head: A, as: List[A]): List[A] =
    Cons(head, tail(as))


  /**
   * EX34.
   * リストの先頭あｋらn個の要素を削除する。
   *
   * @param l
   * @param n
   * @tparam A
   * @return
   */
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(x, y) => drop(y, n - 1)
    }

  /**
   * EX35.
   * fを満たす間lを削除する
   *
   * @param l
   * @param f
   * @tparam A
   * @return
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, y) =>
      if (f(x)) dropWhile(y, f)
      else l
  }

  /**
   * リストの末尾とリストの先頭を紐付ける。
   *
   * @param a1
   * @param a2
   * @tparam A
   * @return
   */
  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  /**
   * EX36.
   * リストの末尾（つまりNil）を発見するまでリストを捜査する必要がある。
   * そのためO(N)の計算量になる。
   * また、引数として与えられたリストのデータを共有することができず、
   * 新しくリストを作成する必要がある。
   *
   * FIXME: case ( ) if g(x) =>
   *
   * @param l
   * @tparam A
   * @return
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B)(implicit print: Boolean): B = as match {
    case Nil => z
    case Cons(x, xs) => if (print) println(s"f(${ x }, foldRight(${ xs }, ${ z })(f))"); f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)


  /**
   * EX37. foldRightを使って実装されたproductは、0.0を検出した場合に直ちに再帰を中止して0.0を返すことはできない。
   * 一度すべてのリストが展開されてすべての要素に関数fが適用されるため。
   * すぐにおもいつくのは、ただちに終了する条件式を関数として指定させ、
   * 満たさなくなった瞬間停止すること。引数が多くてなんだか美しくない…。
   */
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B)(e: B, g: A => Boolean): B = as match {
    case Nil => z
    case Cons(x, xs) if (g(x)) => println(s"immediately stop! found value: $x"); e
    case Cons(x, xs) => println(s"continue. found value $x"); f(x, foldRight2(xs, z)(f)(e, g))
  }

  def product3(ns: List[Double]) =
    foldRight2(ns, 1.0)(_ * _)(0.0, _ == 0)

  /**
   * EX39.
   */
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((x, y) => 1 + y)

  /**
   * EX310
   */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B)(implicit print: Boolean): B = as match {
    case Nil => z
    case Cons(x, xs) => if (print) println(s"foldLeft(${ xs }, f(${ z }, ${ x }))"); foldLeft(xs, f(z, x))(f)
  }

  /**
   * EX311
   */
  def sumL(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productL(l: List[Double]): Double =
    foldLeft(l, 1.0)(_ * _)

  def lengthL(l: List[Any]): Int =
    foldLeft(l, 0)((y, x) => y + 1)

  /**
   * EX312.
   * EX13のために、これを末尾再帰で実行したい。
   * 最初に実装したやつ。末尾再帰ではない。
   */
  def reverse2[A](l: List[A]): List[A] =
    foldRight(l, Nil: List[A])((x, y) => append(y, Cons(x, Nil)))

  /**
   * EX312.
   * foldLeft版のreverse.末尾再帰関数で実装可能。
   */
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((a2: List[A], a1: A) => Cons(a1, a2))

  /**
   * reverseもfoldLeftも末尾再帰。
   */
  def foldRightByFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))

  /**
   * foldRightができれば同じようにやるだけ。
   */
  def foldLeftByFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a: A, b: B) => f(b, a))


  /**
   * EX314.
   * 初期値（つまりNilのかわり）にl2を与える。
   */
  def appendFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))


  /**
   * EX316.
   */
  def addOneEach(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOneEach(xs))
  }

  /**
   * EX317.
   */
  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  /**
   * EX318:map
   */
  def map2[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a: A, b: List[B]) => Cons(f(a), b))

  /**
   * EX319:filter
   */
  def filterOld[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if (!f(x)) => filter(xs)(f)
    case _ => as
  }


  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a: A, b: List[A]) => if (f(a)) Cons(a, b) else b)

  /**
   * EX319:filterOdd
   */
  def filterOdd(as: List[Int]): List[Int] =
    filter(as)(_ % 2 == 0)

  //  def isEven(x: Int): Boolean = x % 2 == 0
  //
  //  def filterOdd(as: List[Int]): List[Int] = as match {
  //    case Nil => Nil
  //    case Cons(x, xs) if (isEven(x)) => Cons(x, filterOdd(filter(xs)(isEven)))
  //    case Cons(x, xs) => filterOdd(filter(xs)(isEven))
  //  }

  /**
   * EX320:flatMap
   */
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a: A, b: List[B]) => append(f(a), b))

  /**
   * EX321:flatMapを使ってfilterを実装せよ。
   */
  
}

