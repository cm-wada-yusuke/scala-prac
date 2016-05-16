package fp.datastuctures

/**
  * ツリー構造を表すトレイトとコンパニオンオブジェクト。
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case t: Leaf => 1
    case t: Branch => (1 + size(t.left)) + (1 + size(t.right))
  }
}