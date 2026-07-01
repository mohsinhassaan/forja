package forja

into opaque type P[T] = P.Erased

object P:
  trait Meta[T]
  object Meta:
    def derived[T]: Meta[T] = ???
  end Meta

  given [T] => (meta: Meta[T]) => Conversion[T, P[T]]:
    def apply(x: T): Erased = ???
  end given

  trait Erased

  extension [T] (p: P[T])
    def fixpoint(fn: PartialFunction[T, T]): P[T] = ???
  end extension
end P
