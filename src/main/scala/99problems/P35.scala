// P35 (**) Determine the prime factors of a given positive integer.
// Construct a flat list containing the prime factors in ascending order.
// scala> 315.primeFactors
// res0: List[Int] = List(3, 3, 5, 7)

package arithmetic {
  class S99Int(val start: Int) {
    import S99Int._

    def isPrime: Boolean =
      (start > 1) && (primes takeWhile { _ <= Math.sqrt(start) } forall { start % _ != 0 })

    def primeFactors: List[Int] = {
      def primeFactorsR(n: Int, ps: Stream[Int]): List[Int] = {
        if (n.isPrime) List(n)
        else if (n % ps.head == 0) ps.head :: primeFactorsR(n / ps.head, ps)
        else primeFactorsR(n, ps.tail)
      }
      primeFactorsR(start, primes)
    }
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })
  }
}
