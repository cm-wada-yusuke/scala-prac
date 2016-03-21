package fp.datastuctures

/**
  * Created by y-wada on 2016/03/21.
  */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

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


  /**
    * EX32. データ構造から考えて、2番目のリストの要素を返せば目的は達成できる。
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
    * EX33. 問の趣旨(matchを使う)から少し外れるかもしれないが、
    * せっかくなので先のtail関数を使うことにする。
    *
    * @param head
    * @param as
    * @tparam A
    * @return
    */
  def setHead[A](head: A, as: List[A]): List[A] =
    Cons(head, tail(as))


  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(x, y) => drop(y, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, y) =>
      if (f(x)) dropWhile(y, f)
      else l
  }

}

