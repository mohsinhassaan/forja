package forja

final class Sum[T](private[forja] val meta: Lang2.SumMeta[T]) extends AnyVal

object Sum:
  inline def derived[T]: Sum[T] =
    ${ MetaMacros.sumDerivedImpl[T] }
  end derived
end Sum
