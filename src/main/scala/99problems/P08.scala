// P08 (**) Eliminate consecutive duplicates of list elements.
// If a list contains repeated elements they should be replaced with a
// single copy of the element. The order of the elements should not be
// changed.
// Example:
//
// scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

object P08 {
  // Standard Recursive
  def compressRecursive[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
  }

  // Tail Recursive
  def compressTailRecursive[A](list: List[A]): List[A] = {
    def compressR[A](result: List[A], curList: List[A]): List[A] = curList match {
      case Nil => result.reverse
      case h :: tail => compressR(h :: result, tail.dropWhile(_ == h))
    }
    compressR(Nil, list)
  }
  // def compressTailRecursive[A](list: List[A]): List[A] = {
  //   def compressR[A](result: List[A], curList: List[A]): List[A] = curList match {
  //     case Nil => result.reverse
  //     case h :: tail => {
  //       if (result.isEmpty || result.head != h) compressR(h :: result, tail)
  //       else compressR(result, tail)
  //     }
  //   }
  //   compressR(Nil, list)
  // }

  // Pure functional
  def compressFunctionalLeft[A](list: List[A]): List[A] =
    list.foldLeft(List(list.head)) { (a, b) => if (b != a.head) b :: a else a }.reverse

  def compressFunctionalRight[A](list: List[A]): List[A] =
    list.foldRight(List[A]()) { (h, r) =>
      if (r.isEmpty || r.head != h) h :: r else r
    }
}
