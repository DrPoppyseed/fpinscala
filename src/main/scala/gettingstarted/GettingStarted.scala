package gettingstarted

object MyProgram:
  def abs(n: Int): Int =
    if n < 0 then -n
    else n

  def factorial(n: Int): Int =
    def go(n: Int, acc: Int): Int =
      if n <= 0 then acc
      else go(n - 1, n * acc)

    go(n, 1)

  def fib(n: Int): Int =
    @annotation.tailrec
    def go(n: Int, prev: Int, cur: Int): Int =
      if n <= 0 then prev
      else go(n - 1, cur, prev + cur)

    go(n, 0, 1)

  private def formatAbs(x: Int) =
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))

  private def formatFactorial(n: Int) =
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))

  private def formatFib(n: Int) =
    val msg = "The %dth fibonacci number is %d."
    msg.format(n, fib(n))

  @main def print: Unit =
    println(formatAbs(-42))
    println(formatFactorial(7))
    println(formatFib(10))

object PolymorphicFunctions:
  def findFirst[A](as: Array[A], p: A => Boolean): Int =
    @annotation.tailrec
    def loop(n: Int): Int =
      if n >= as.length then -1
      else if p(as(n)) then n
      else loop(n + 1)

    loop(0)

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n + 1 >= as.length) true
      else if (gt(as(n), as(n + 1))) false
      else loop(n + 1)

    loop(0)

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

