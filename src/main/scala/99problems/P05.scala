// P05 (*) Reverse a list.
// Example:
// scala> reverse(List(1, 1, 2, 3, 5, 8))
// res0: List[Int] = List(8, 5, 3, 2, 1, 1)

object P05 {
  // Builtin
  def reverseBuiltin[A](list: List[A]): List[A] = list.reverse

  // Recursive
  def reverseRecursive[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case h :: tail => reverseRecursive(tail) ::: List(h)
  }

  // Tail recursive
  def reverseTailRecursive[A](list: List[A]): List[A] = {
    def reverseR(result: List[A], curList: List[A]): List[A] = curList match {
      case Nil => result
      case h :: tail => reverseTailR(h :: result, tail)
    }
    reverseR(Nil, list)
  }

  // Pure Functional
  def reverseFunctional[A](list: List[A]): List[A] = list.foldLeft(List[A]()){ (r, h) => r :: h }
}
