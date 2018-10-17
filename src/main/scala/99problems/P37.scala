class S99Int(val start: Int) {
  def totient: Int =
    start.primeFactorMultiplicity.foldLeft(1) {
      (r, f) =>
        f match {
          case (p, m) => r * (p - 1) * Math.pow(p, m - 1).toInt
        }
    }
}
