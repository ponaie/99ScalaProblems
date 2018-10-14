// P36 (**) Determine the prime factors of a given positive integer (2).
// Construct a list containing the prime factors and their multiplicity.
// scala> 315.primeFactorMultiplicity
// res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
// Alternately, use a Map for the result.
//
// scala> 315.primeFactorMultiplicity
// res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)

package arithmetic {
  class S99Int(val start: Int) {
    import S99Int._

    def isPrime: Boolean =
      (start > 1) && (primes takeWhile { _ <= Math.sqrt(start)  } forall { start % _ != 0  })

    def primeFactors: List[Int] = {
      def primeFactorsR(n: Int, ps: Stream[Int]): List[Int] =
        if (n.isPrime) List(n)
        else if (n % ps.head == 0) ps.head :: primeFactorsR(n / ps.head, ps)
        else primeFactorsR(n, ps.tail)
      primeFactorsR(start, primes)
    }

    def primeFactorMultiplicity: Map[Int,Int] = {
      def factorCount(n: Int, p: Int): (Int,Int) =
        if (n % p != 0) (0, n)
        else factorCount(n / p, p) match { case (c, d) => (c + 1, d)  }
      def factorsR(n: Int, ps: Stream[Int]): Map[Int, Int] =
        if (n == 1) Map()
        else if (n.isPrime) Map(n -> 1)
        else {
          val nps = ps.dropWhile(n % _ != 0)
          val (count, dividend) = factorCount(n, nps.head)
          Map(nps.head -> count) ++ factorsR(dividend, nps.tail)
        }
      factorsR(start, primes)
    }
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime  })
}
