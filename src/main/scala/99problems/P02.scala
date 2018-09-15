// P02 (*) Find the last but one element of a list.
// Example:
// scala> penultimate(List(1, 1, 2, 3, 5, 8))
// res0: Int = 5

object P02 {
  // Built-in
  def penultimateBuiltin[A](list: List[A]): A = {
    if(list.isEmpty) throw new NoSuchElementException
    else list.init.last
  }

  // Recursive
  def penultimateRecursive[A](list: List[A]): A = list match {
    case e :: _ :: Nil => e
    case _ :: tail => penultimateRecursive(tail)
    case _ => NoSuchElementException
  }
}
