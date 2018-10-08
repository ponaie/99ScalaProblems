// P23 (**) Extract a given number of randomly selected elements from a list.
// Example:
// scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
// res0: List[Symbol] = List('e, 'd, 'a)
// Hint: Use the solution to problem P20

object P23 {
  import P20.removeAt

  def randomSelect[A](n: Int, ls: List[A]): List[A] = {
    def randomSelectR[A](c: Int, curList: List[A], result: List[A], r: util.Random): List[A] =
      (c, curList) match {
        case (_, Nil) => result
        case (0, _) => result
        case (cnt, l) => {
          val (rest, p) = removeAt(r.nextInt(l.length), l)
          randomSelectR(cnt - 1, rest, p :: result, r)
        }
      }
    randomSelectR(n, ls, Nil, new util.Random)
  }
}
