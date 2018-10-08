// P02 (*) Find the last but one element of a list.
// Example:
// scala> penultimate(List(1, 1, 2, 3, 5, 8))
// res0: Int = 5

object P02 {
  // Built-in
  def penultimateBuiltin[A](list: List[A]): A = {
    if (list.isEmpty) throw new NoSuchElementException
    else list.init.last
  }

  // Recursive
  def penultimateRecursive[A](list: List[A]): A = list match {
    case e :: _ :: Nil => e
    case _ :: tail => penultimateRecursive(tail)
    case _ => NoSuchElementException
  }

  // lastNth Builtin
  def lastNthBuiltin[A](n: Int, ls: List[A]): A = {
    if (n <= 0) throw new IllegalArgumentException
    if (ls.length < n) throw new NoSuchElementException
    ls.takeRight(n).head
  }

  // Recursive(similar to two pointers approach)
  def lastNthRecursive[A](n: Int, ls: List[A]): A = {
    def lastNthR(count: Int, resultList: List[A], curList: List[A]): A =
      curList match {
        case Nil if count > 0 => throw new NoSuchElementException
        case Nil => resultList.head
        case _ :: tail =>
          lastNthR(
            count - 1,
            if (count > 0) resultList else resultList.tail,
            tail)
      }
    if (n <= 0) throw new IllegalArgumentException
    else lastNthR(n, ls, ls)
  }
}
