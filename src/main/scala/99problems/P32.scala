// P32 (**) Determine the greatest common divisor of two positive integer numbers.
// Use Euclid's algorithm.
// scala> gcd(36, 63)
// res0: Int = 9

package arithmetic {
  class S99Int(val start: Int) {
    import S99Int._

  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)
  }

}
