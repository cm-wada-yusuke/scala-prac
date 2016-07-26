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

  def unfoldMap[B](f: A => B): Stream[B] =
    InfiniteStream.unfold(this) {
      case Empty => None // 末尾まで来たら終了
      case Cons(h, t) => Some((f(h()), t())) // 先頭にfを適用して次へ
    }

  def unfoldTake(n: Int): Stream[A] =
    InfiniteStream.unfold((n, this)) {
      case (x, Cons(h, t)) if x > 0 => Some((h(), (x - 1, t()))) // x>0なら先頭を使って新ストリームを構築
      case _ => None // それ以外は終了
    }

  def unfoldTakeWhile(f: A => Boolean): Stream[A] =
    InfiniteStream.unfold(this) {
      case Cons(h, t) if f(h()) => Some((h(), t())) // 先頭がfを満たしていたらStream作成
      case _ => None // それ以外は終了
    }

  def zipWith[B](s2: Stream[B])(f: (A, B) => B): Stream[B] =
    InfiniteStream.unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2()))) // どちらも先頭を持っている場合、それらを引数とするfを適用、次の状態は残りのストリームのタプル
      case _ => None // どちらか片方のStreamが尽きた時点で終了
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    InfiniteStream.unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2()))) // Some(次の値,次の状態)どちらも要素を持ってる場合、それらを引数とするfを適用、次の状態は残りのストリームのタプル
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), Stream.empty))) // 片方が空の場合、空側はNoneを返すようにし、残ったほうで続ける
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (Stream.empty, t2())))
      case _ => None // 両方のStreamが尽きたら終了
    }

  /**
   * zipWithは、thisのほうが短い場合に対応できない（意図と反してtrueになってしまう場合がある）ため候補から外れた。
   * falseを発見した瞬間に強制終了するfoldRightがポイントになる…はず。
   */
  def startsWith[B >: A](s: Stream[B]): Boolean =
  zipAll(s).foldRight(true) {
    case (_, false) => false // 一度falseになったらタプル値にかかわらず最後までfalse
    case ((None, _), _) => false // thisが先に尽きてしまった場合はfalse
    case ((Some(a), None), _) => true // thisのみの場合は次の調査へ進む
    case ((Some(a), Some(b)), _) if a == b => true // どちらの要素もあり、かつ等しい場合次の調査へ進む
    case ((Some(a), Some(b)), _) => false // 要素が一致しない瞬間調査終了
  }

  def tails: Stream[Stream[A]] =
    InfiniteStream.unfold(this) {
      case Cons(h, t) => Some((Cons(h, t), t()))
      case Empty => None
    }

  // scanRight
  // Stream(1,2,3).scanRight(0)(_ + _ ) => List(6,5,3,0)

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

object InfiniteStream {

  def constant[X](a: X): Stream[X] = {
    lazy val tail: Stream[X] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def sub(p: Int, n: Int): Stream[Int] = Stream.cons(p, sub(n, p + n)) // 美しい…
    sub(0, 1)
  }

  def unfold[X, S](z: S)(f: S => Option[(X, S)]): Stream[X] = f(z) match {
    case None => Stream.empty
    case Some((v, s)) => Stream.cons(v, unfold(s)(f))
  }

  def unfoldFibs: Stream[Int] = unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))

  def unfoldFrom(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def unfoldConstant[X](a: X): Stream[X] = unfold(a)(s => Some((a, a)))

  def unfoldOnes: Stream[Int] = unfold(1)(s => Some((1, 1)))
}
