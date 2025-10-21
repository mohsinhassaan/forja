package forja.util

trait MonomorphicIndexedSeq[+T, C <: MonomorphicIndexedSeq[T, C]]
    extends IndexedSeq[T]:
  self: C =>
  final override def slice(from: Int, until: Int): C = sliceImpl(from, until)
  protected def sliceImpl(from: Int, until: Int): C

  final override def tail: C = drop(1)
  final override def tails: Iterator[C] =
    Iterator.unfold(Some(this): Option[C]):
      case None      => None
      case Some(acc) =>
        if acc.isEmpty
        then Some((acc, None))
        else Some((acc, Some(acc.tail)))
  end tails
  final override def init: C = dropRight(1)
  final override def inits: Iterator[C] =
    Iterator.unfold(Some(this): Option[C]):
      case None      => None
      case Some(acc) =>
        if acc.isEmpty
        then Some((acc, None))
        else Some((acc, Some(acc.init)))
  end inits

  final override def drop(n: Int): C = slice(n, length)
  final override def dropRight(n: Int): C = slice(0, length - n)

  final override def take(n: Int): C = slice(0, n)
  final override def takeRight(n: Int): C = slice(length - n, length)
end MonomorphicIndexedSeq
