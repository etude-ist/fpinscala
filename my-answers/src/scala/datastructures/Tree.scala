sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // EXERCISE 25: Write a function size that counts the number of nodes in a tree. 
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // EXERCISE 26: Write a function maximum that returns the maximum element
  // in a Tree[Int].
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // EXERCISE 27: Write a function depth that returns the maximum path length
  // from the root of a tree to any leaf.
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // EXERCISE 28: Write a function map, analogous to the method of the same name
  // on List, that modifies each element in a tree with a given function
  def mapTree[A, B](tree: Tree[A])(fn: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(mapTree(l)(f), mapTree(r)(f))
  }

}
