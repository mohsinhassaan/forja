package forja.util

abstract class InlineConversion[-T, +U] extends Conversion[T, U]:
  inline def apply(t: T): U
end InlineConversion

object InlineConversion:
  sealed abstract class ByCast[-T, +U] private extends InlineConversion[T, U]:
    inline def apply(t: T): U = t.asInstanceOf[U]
  end ByCast
  object ByCast:
    inline def apply[T, U](): ByCast[T, U] = null.asInstanceOf
  end ByCast
end InlineConversion
