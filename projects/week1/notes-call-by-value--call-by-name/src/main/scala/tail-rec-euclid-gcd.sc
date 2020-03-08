import scala.annotation.tailrec

// Euclid's algorithm
@tailrec
def gcd(a: Int, b: Int):Int =
  if (b==0) a else gcd(b, a % b) // last action is call-self = tail recursion which is efficient as iteration (const stack size)

// vs factorial which is NOT tail-recursive

gcd(6,3)
gcd(100, 70)
