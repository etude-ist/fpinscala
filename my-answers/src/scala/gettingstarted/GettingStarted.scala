object MyModule {

  def factorial(n: Int): Int = {

    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }

    go(n, 1)

  }


  // EXERCISE 1 (optional): Write a function to get the nth Fibonacci number. 
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

  

}
