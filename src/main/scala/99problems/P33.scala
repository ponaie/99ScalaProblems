// P33 (*) Determine whether two positive integer numbers are coprime.
// Two numbers are coprime if their greatest common divisor equals 1.
// scala> 35.isCoprimeTo(64)
// res0: Boolean = true

package arithmetic {
  class S99Int(val start: Int) {
    import S99Int._

    def isCoprimeTo(n: Int): Boolean = gcd(start, n) == 1
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)
  }
}
