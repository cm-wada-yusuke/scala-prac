package fp.datastuctures.stream

import scala.reflect.ClassTag

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

  // TODO :: を使って連結する
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => List(h()) ++ t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n == 0 => Empty
    case Cons(h, t) => Cons(h, () => t().take(n - 1))
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if n == 0 => t()
    case Cons(h, t) => t().drop(n - 1)
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if !f(h()) => Empty
    //    case Cons(h, t) if !f(h()) => Cons(h, () => Empty)
    case Cons(h, t) => Cons(h, () => t().takeWhile(f))
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def map[B](f: A => B): Stream[B] =
    this.foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((h, t) =>
      if (f(h)) {
        Stream.cons(h, t)
      } else {
        t
      })

  def append[B >: A](b: Stream[B]): Stream[B] =
    this.foldRight(b)((h, t) => Stream.cons(h, t))


  //  def append(a: Stream[A]): Stream[A] =
  //    this.foldRight(a)((h, t) => Stream.cons(h, t))

  /**
    * abstract type pattern A is unchecked since it is eliminated by erasure
    */
  //  def append[B >: A : ClassTag](b: Stream[B]): Stream[B] =
  //    b.headOption match {
  //      case None => this
  //      case Some(head) => head match {
  //        case x: A => this.foldRight(b)((h, t) => Stream.cons(h, t))
  //        case _ => this
  //      }
  //    }

  //    if ((b.type) ==  (this.type)) {
  //      this.foldRight(b)((h, t) => Stream.cons(h, t))
  //    }
  //    else {
  //      this
  //    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Stream.empty[B])((a, b) => f(a).append(b))

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

object InfinityStream {
  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def sub(p: Int, n: Int): Stream[Int] = Stream.cons(p, sub(n, p + n)) // 美しい…
    sub(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Stream.empty
    case Some((v, s)) => Stream.cons(v, unfold(s)(f))
  }

  def unfoldFibs: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def unfoldFrom(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def unfoldConstant[A](a: A): Stream[A] = unfold(a)(s => Some((a, a)))

  def unfoldOnes: Stream[Int] = unfold(1)(s => Some((1, 1)))
}
