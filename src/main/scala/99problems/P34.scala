// P34 (**) Calculate Euler's totient function phi(m).
// Euler's so-called totient function phi(m) is defined as the number of
// positive integers r (1 <= r <= m) that are coprime to m.
// scala> 10.totient
// res0: Int = 4

package arithmetic {
  class S99Int(val start: Int) {
    import S99Int._

    def isCoprimeTo(n: Int): Boolean = gcd(start, n) == 1

    def totient: Boolean = (1 to start) count { start.isCoprimeTo(_) }
    // def totient: Boolean = (1 to start) filter { start.isCoprimeTo(_) } length
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)
  }
}
