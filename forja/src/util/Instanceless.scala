package forja.util

import scala.annotation.implicitNotFound

trait Instanceless(using poison: Instanceless.Poison)

object Instanceless:
  inline def apply[T]: T = null.asInstanceOf[T]

  @implicitNotFound("this trait should never have a meaningful instance")
  opaque type Poison = Nothing
end Instanceless
