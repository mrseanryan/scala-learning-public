import scala.annotation.tailrec

// this will not compile: @tailrec
def factorial(n: Int): Int =
  if (n == 0) 1 else n * factorial(n-1) // last action is NOT self-call, (x * self), so stack grows (not tail-recursive)

def factorial_tailrec(n:Int):Int = {
  @tailrec
  def fac_int(from: Int, to: Int, acc: Int): Int = {
    if (from > to)
      acc
    else
      fac_int(from + 1, to, acc * from)
  }

  fac_int(1, n, 1)
}

def fact_simpler(n: Int):Int = {
  @tailrec
  def loop(acc:Int, n:Int): Int = {
    if (n==0)
      acc
    else
      loop(n*acc, n-1)
  }

  loop(1, n)
}

factorial(3)
factorial_tailrec(3)
fact_simpler(3)

factorial(4)
factorial_tailrec(4)
fact_simpler(4)

factorial(5)
factorial_tailrec(5)
fact_simpler(5)

