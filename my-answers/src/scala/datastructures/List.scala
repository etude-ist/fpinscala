package datastructures

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

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)

  // Exercise 1: What will the result of the following match expression be?
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
  // List(1,2,3,4,5) matches Cons(x, Cons(y, Cons(3, Cons(4, _)))) so the result
  // of the following match expression will be 3.

  // Exercise 2: Implement the function tail for "removing" the first element
  // of a List.
  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => sys.error("expected argument of type non-empty list")
    case Cons(_, xs) => xs
  }

  // Exercise 3: Generalize tail to the function drop, which removes the first
  // n elements from a list.
  def drop[A](n: Int, lst: List[A]): List[A] = (lst, n) match {
    case (Nil, _) => Nil
    case (_, 0) => lst
    case (Cons(_, xs), _) => drop(n-1, xs)
  }

  // Exercise 4: Implement dropWhile, which removes elements from the
  // List prefix as long as they match a predicate.
  def dropWhile[A](lst: List[A])(pred: A => Boolean): List[A] = lst match {
    case Cons(x, xs) if pred(x) => dropWhile(xs)(pred)
    case _ => lst
  }

  // Exercise 5: Implement the function setHead for
  // replacing the first element of a List with a different value.
  def setHead[A](elt: A, lst: List[A]): List[A] = lst match {
    case Nil => sys.error("expected argument of type non-empty list")
    case Cons(_, xs) => Cons(elt, xs)
  }

}
