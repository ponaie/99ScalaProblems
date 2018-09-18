// P03 (*) Find the Kth element of a list.
// By convention, the first element in the list is element 0.
// Example:
//
// scala> nth(2, List(1, 1, 2, 3, 5, 8))
// res0: Int = 2

object P03 {
  // Built-in
  def nthBuiltin[A](n: Int, list: List[A]): A = {
    if (n<0) throw new IllegalArgumentException
    if (n > list.length) throw new NoSuchElementException
    list.drop(n).head
  }

  // Recursive
  def nthRecursive[A](n: Int, list: List[A]): A = (n, list) match {
    case (0, h :: _) => h
    case (n, _ :: tail) => nthRecursive(n - 1, tail)
    case (_, Nil) => throw new NoSuchElementException
    }
}
