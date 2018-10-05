// P18 (**) Extract a slice from a list.
// Given two indices, I and K, the slice is the list containing the elements
// from and including the Ith element up to but not including the Kth element
// of the original list. Start counting the elements with 0.
// Example:
//
// scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: List[Symbol] = List('d, 'e, 'f, 'g)

object P18 {

  // Builtin
  def sliceBuiltin[A](s: Int, e: Int, ls: List[A]): List[A] =
    ls.slice(s, e)

  // Tail Recursive
  def sliceRecusive[A](s: Int, e: Int, ls: List[A]): List[A] = {
    def sliceR[A](count: Int, curList: List[A], result: List[A]): List[A] =
      (count, curList) match {
        case (_, Nil) => result.reverse
        case (c, h :: tail) if end <= c => result.reverse
        case (c, h :: tail) if start <= c => sliceR(c + 1, tail, h :: result)
        case (c, _ :: tail) => sliceR(c + 1, tail,result)
    }
    sliceR(0, ls, Nil)
  }

  // Functional
  def sliceFunctional[A](s: Int, e: Int, ls: List[A]): List[A] =
    ls.drop(s).take(e - (s max 0))
}
