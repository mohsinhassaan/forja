package forja

import scala.annotation.publicInBinary

final class Term[T] @publicInBinary private[forja] (private[forja] val meta: Lang2.TermMeta[T]) extends AnyVal

object Term:
  inline def derived[T]: Term[T] =
    ${ MetaMacros.termDerivedImpl[T] }
  end derived
end Term
