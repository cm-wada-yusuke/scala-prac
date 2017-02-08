package dwango.caseclass

sealed abstract class Tree

case class Branch(value: Int, left: Tree, right: Tree) extends Tree

case object Empty extends Tree

object TreeExecutor {

  def main(args: Array[String]): Unit = {
    val t: Tree = Branch(1, Branch(2, Empty, Empty), Branch(3, Empty, Empty))
    println(max(t))
    println(min(t))

    val t1 = Branch(10, Empty, Empty)
    val t2 = Branch(10, Branch(20, Empty, Empty), Empty)
    val t3 = Branch(10, Branch(20, Empty, Empty), Branch(30, Branch(40, Empty, Empty), Empty))
    val t4 = Branch(34, Branch(52, Empty, Empty), Branch(11, Branch(5, Empty, Empty), Empty))

    println(depth(Empty))
    println(depth(t1))
    println(depth(t2))
    println(depth(t3))

    println(sort(t3))
    println(sort(t4))
  }

  private def toList(tree: Tree): List[Int] = tree match {
    case Branch(v, l: Tree, r: Tree) => v +: toList(l) ::: toList(r)
    case Empty => List.empty[Int]
  }

  def max(tree: Tree): Int = toList(tree).max

  def min(tree: Tree): Int = toList(tree).min

  private def maxInt(a: Int, b: Int): Int = if (a > b) a else b

  def depth(tree: Tree): Int = tree match {
    case Empty => 0
    case Branch(_, l: Tree, r: Tree) => 1 + maxInt(depth(l), depth(r))
  }

  def add(tree: Tree, i: Int): Tree = tree match {
    case Empty => Branch(i, Empty, Empty)
    case Branch(v, Empty, Empty) => if (i <= v) Branch(v, Branch(i, Empty, Empty), Empty) else Branch(v, Empty, Branch(i, Empty, Empty))
    case Branch(v, l: Tree, r: Tree) => if (i <= v) Branch(v, add(l, i), r) else Branch(v, l, add(r, i))
  }

  def sort(tree: Tree): Tree = toList(tree).foldLeft(Empty: Tree)(add)
}