// P09 (**) Pack consecutive duplicates of list elements into sublists.
// If a list contains repeated elements they should be placed in separate sublists.
// Example:
//
// scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c),
// List('a, 'a), List('d), List('e, 'e, 'e, 'e))

object P09 {

  // Tail recursive
  def packTailRecursive[A](list: List[A]): List[List[A]] = {
    def packR[A](result: List[List[A]], curList: List[A]): List[List[A]] = curList match {
      case Nil => result.reverse
      case h :: tail =>  packR(curList.takeWhile(_ == h) :: result, tail.dropWhile(_ == h))
    }
    packR(Nil, list)
  }
}
