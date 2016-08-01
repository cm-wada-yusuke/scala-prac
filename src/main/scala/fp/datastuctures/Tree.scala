package fp.datastuctures

/**
  * ツリー構造を表すトレイトとコンパニオンオブジェクト。
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def string[A](t: Tree[A]): String = {
    def printNode(acc: Tree[A]): String = acc match {
      case Leaf(v) => s"[${v.toString}]"
      case Branch(l, r) => "[ + ]" + s"${string(l)} ${string(r)}"
    }
    printNode(t)
  }

  /**
    * EX
    */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /**
    * 二分木のルートから任意のLeafまでの最長パスを返すdepthを定義せよ。
    * （複数ある場合はより大きい方を採用する。）
    * （見つからない場合は0を返す）
    */
  def depth[A](t: Tree[A], s: A): Int = {
    def loop(acc: Tree[A], depth: Int): Int = acc match {
      case Leaf(v) if s == v => depth
      case Leaf(v) => 0
      case Branch(l, r) => loop(l, depth + 1) max loop(r, depth + 1)
    }
    loop(t, 0)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def foldSize[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def foldMaximum(t: Tree[Int]): Int =
    fold(t)(v => v)((l, r) => l max r)

  def foldDepth[A](t: Tree[A], s: A): Int = {
    def loop(acc: Tree[A], depth: Int): Int =
      fold(acc)(v => if (v == s) depth else 0)((l, r) => l max r)
    loop(t, 0)
  }

  def foldMap[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(v => Leaf(f(v)): Tree[B])((l, r) => Branch(l, r))

}