// P17 (*) Split a list into two parts.
// The length of the first part is given. Use a Tuple for your result.
// Example:
//
// scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))

object P17 {

  // Builtin
  def splitBuiltin[A](n: Int, ls: List[A]): (List[A], List[A]) = ls.splitAt(n)

  // Tail Recursive
  def splitRecusive[A](n: Int, ls: List[A]): (List[A], List[A]) = {
    def splitR[A](n: Int, preList: List[A], curList: List[A]): (List[A], List[A]) = (n, curList) match {
      case (0, list) => (preList.reverse, list)
      case (_, Nil) => (preList.reverse, Nil)
      case (c, h :: tail) => splitR(c - 1, h :: preList, tail)
    }
    splitR(n, Nil, ls)
  }

  // Functional
  def splitFunctional[A] (n: Int, ls: List[A]): (List[A], List[A]) =
    (ls.take(n), ls.drop(n))
}
