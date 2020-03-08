// Newton-Rapson method to find a square root
def sqrt(x: Double):Double = {
  def improve(guess:Double) =
    (guess + x / guess ) / 2

  def isGoodEnough(guess: Double) =
    //abs(guess * guess - x) < 0.01 // relatively too big for smalls, and could be smaller than gaps between large floating points (mantissa?)
    //abs(guess * guess - x) / x < 0.01 // a bit better
    improve(guess) == guess // can not be improved, given this machines precision!

  def sqrIter(guess: Double): Double =
    if (isGoodEnough(guess)) guess
    else sqrIter(improve(guess))


  sqrIter(1.0)
}

def testSqrt(x: Double) = {
  val sqrResult = sqrt(x)
  val error = (sqrResult * sqrResult) - x
  println(s"sqrt($x) = {$sqrResult} Error: {$error}")
}

testSqrt(25)
testSqrt(4)
testSqrt(2)
testSqrt(10000)

// more tricky - small and big values (problem with epsilon)
testSqrt(0.001)
testSqrt(1e-20)
testSqrt(1e20)
testSqrt(0.1e50)
testSqrt(1e60)
