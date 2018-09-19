// P04 (*) Find the number of elements of a list.
// Example:
// scala> length(List(1, 1, 2, 3, 5, 8))
// res0: Int = 6

object P04 {
  // Builtin
  def lengthBuiltin[A](list: List[A]): Int = list.length

  // Recursive
  def lengthRecursive[A](list: List[A]): Int = {
    ls match {
      case Nil => 0
      case _ :: tail => 1 + lengthRecursive(tail)
    }
  }

  def lengthTailRecursive[A](list: List[A]): Int = {
    def lengthR[A](cnt: Int, list: List[A]): Int = {
      ls match {
        case Nil => cnt
        case _ :: tail => lengthR(cnt + 1, tail)
      }
    }
    lengthR(0, list)
  }

  def lengthFunctional[A](list: List[A]): Int = list.foldLeft(0) { (c, _) => c + 1 }
}
