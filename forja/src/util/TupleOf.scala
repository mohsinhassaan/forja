package forja.util

import scala.compiletime.summonAll

final class TupleOf[T <: Tuple](val value: T) extends AnyVal

object TupleOf:
  inline given tupleOf: [T <: Tuple] => TupleOf[T] = TupleOf(summonAll[T])
end TupleOf
