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

  // Exercise 6: Implement a function init, which returns a List
  // consisting of all but the last element of a List.
  // Example: List(1,2,3,4) => List(1,2,3)
  def init[A](lst: List[A]): List[A] = lst match {
    case Nil => sys.error("expected argument of type non-empty list")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  // Exercise 7: Can product implemented using foldRight immediately
  // halt the recursion and return 0.0 if it encounters a 0.0? Why or why not?
  // Answer: No, because it must push frames onto the call stack as we go
  // before it can begin collapsing it.

  // Exercise 8: See what happens when you pass Nil and Cons themselves to
  // foldRight:
  // scala> List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _))
  // res0: List[Int] = Cons(1,Cons(2,Cons(3,Nil)))

  // Exercise 9: Compute the length of a list using foldRight
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }

  // Exercise 10: foldRight is not tail-recursive and will StackOverflow
  // for large lists. Convince yourself that this is the case, then write
  // another general list-recursion function, foldLeft that is tail-recursive,
  // using techniques discussed in the previous chapter.
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go(lst: List[A], acc: B): B = {
      lst match {
        case Nil => acc
        case Cons(x, xs) => go(xs, f(acc, x))
      }
    }
    go(l, z)
  }

  // Exercise 11: Write sum, product, and a function to compute the length of
  // a list using foldLeft.
  def sumFL(l: List[Int]) =
    foldLeft(l, 0)(_ + _)

  def productFL(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  def lengthFL[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc, _) => acc + 1)
  }

  // Exercise 12: Write a function that returns the reverse of a list (so given
  // List(1,2,3) it returns List(3,2,1). See if you can write it using a fold.
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
  }

  // Exercise 14: Implement append in terms of either foldLeft or foldRight.
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  // Exercise 15: Write a function that concatenates a list of lists into a single list.
  // Its runtime should be linear in the total length of all lists. Try to use
  // functions we have already defined.
  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, List[A]())((acc, h) => append(acc, h))
  }

}
