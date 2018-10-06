// P22 (*) Create a list containing all integers within a given range.
// Example:
// scala> range(4, 9)
// res0: List[Int] = List(4, 5, 6, 7, 8, 9)

object P22 {

  // Builtin
  def rangeBuiltin(start: Int, end: Int): List[Int] =
    List.range(start, end + 1)

  // Tail Recursive
  def rangeRecursive(start: Int, end: Int): List[Int] = {
    def rangeR(e: Int, result: List[Int]): List[Int] = {
      if (e < start) result
      else rangeR(e - 1, e :: result)
    }
    rangeR(end, Nil)
  }
}
