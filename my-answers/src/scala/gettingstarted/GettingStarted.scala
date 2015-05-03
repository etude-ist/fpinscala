object GettingStarted {

  def abs(n: Int): Int = {
    if (n < 0) {
      -n 
    } else {
      n
    }
  }

  def factorial(n: Int): Int = {

    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }

    go(n, 1)

  }


  // EXERCISE 1: Write a function to get the nth Fibonacci number. 
  // The first two Fibonacci numbers are 0 and 1, and the next number is always 
  // the sum of the previous two. Your definition should use a local tail-recursive function.
  def fib(n: Int): Int = {

    def fibAux(a: Int, b: Int, n: Int): Int = {
      if (n == 0) {
        a
      } else {
        fibAux(b, a+b, n-1)
      }
    }

    fibAux(0, 1, n)

  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }


  // Binary search as an example of monomorphic function
  def monoBinarySearch(ds: Array[Double], key: Double): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val d = ds(mid2)
        if (d == key) mid2
        else if (d > key) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, ds.length - 1)
  }


  // Binary search rewritten to serve as an example of polymorphic function
  def polyBinarySearch[A](as: Array[A], key: A, gt: (A, A) => Boolean): Int = {
    @annotation.tailrec
    def go(low: Int, mid: Int, high: Int): Int = {
      if (low > high) -mid - 1
      else {
        val mid2 = (low + high) / 2
        val a = as(mid2)
        val greater = gt(a, key)
        if (!greater && !gt(key, a)) mid2
        else if (greater) go(low, mid2, mid2-1)
        else go(mid2 + 1, mid2, high)
      }
    }
    go(0, 0, as.length - 1)
  }

  // EXERCISE 2: Implement isSorted, which checks whether an Array[A] is
  // sorted according to a given comparison function
  def isSorted[A](arr: Array[A], comp: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(first: Int, next: Int, last: Int): Boolean = {
      if (next > last) true
      else {
        val x = arr(first)
        val y = arr(next)
        if (comp(x, y)) {
          go(next, next+1, last)
        } else {
          false
        }
      }
    }
    val n = arr.length - 1
    if (n >= 2) {
      go(0, 1, n)
    } else {
      true
    }
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("fibonacci", 10, fib))
  }

}
