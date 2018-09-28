// P10 (*) Run-length encoding of a list.
// Use the result of problem P09 to implement the so-called run-length encoding
// data compression method. Consecutive duplicates of elements are encoded as
// tuples (N, E) where N is the number of duplicates of the element E.
// Example:
//
// scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))')')')')')'))

object P10 {
  import P09.pack

  def encode[T](ls: List[T]): List[(Int, T)] =
    pack(ls) map { (e.length, e.head) }

  // def encode[T](list: List[T]): List[(Int, T)] =
  //   pack(ls) foldRight (List[(Int, T)]()) { (e, z) => (e.length, e.head) :: z }
}
