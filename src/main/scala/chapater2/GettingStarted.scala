package chapater2

object GettingStarted {

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  def fib(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n: Int, acc0: Int, acc1: Int): Int =
      if (n == 1) acc0
      else go(n-1, acc1, acc0+acc1)

    go(n, 0, 1)
  }

  def factorial(n: Int): Int = {
    @scala.annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s value of %d is %d"
    msg.format(name, n, f(n))
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(n: Int): Boolean = {
      if (as.length-1 <= n) true
      else if (ordered(as(n), as(n+1))) false
      else loop(n+1)
    }

    loop(0)
  }

  def main(args: Array[String]): Unit = {
    println(formatResult("absolute", -42, abs))
    println(formatResult("fib", 3, fib))
  }
}
