// P31 (**) Determine whether a given integer number is prime.
// scala> 7.isPrime
// res0: Boolean = true

package arithmetic {
  class S99Int(val start: Int) {
    import S99Int._

    def isPrime: Boolean =
      (start > 1) && (primes takeWhile { _ <= Math.sqrt(start) } forall { start % _ != 0 })
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })
  }
}
