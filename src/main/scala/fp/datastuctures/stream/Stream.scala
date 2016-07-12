package fp.datastuctures.stream

/**
  * 遅延リスト
  */
trait Stream[+A] {

  def headOption = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  //  def toList: List[A] = {
  //    def loop[A](ss: Stream[A]): List[A] = ss match {
  //      case Empty => Nil
  //      case Cons(h, t) => List(h()) ++ loop(t())
  //    }
  //    loop(this)
  //  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => List(h()) ++ t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n == 0 => Empty
    case Cons(h, t) => Cons(h, () => t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = {
    def loop[A](acc: Stream[A], n: Int): Stream[A] = acc match {
      case Empty => Empty
      case Cons(h, t) if n == 0 => acc
      case Cons(h, t) => loop(t(), n - 1)
    }
    loop(this, n)
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

