package forja.util

import scala.compiletime.deferred
import scala.jdk.CollectionConverters.*
import scala.quoted.{Expr, Quotes, Type, Varargs, quotes}
import scala.reflect.{ClassTag, TypeTest}

import ReflectiveEnumeration.*

transparent trait ReflectiveEnumeration:
  reflectiveEnumeration =>
  private[ReflectiveEnumeration] given fieldList
      : FieldList[reflectiveEnumeration.type] = deferred

  def valuesByType[T <: Enumerable: ClassTag](using
      TypeTest[Enumerable, T],
  ): IArray[(String, T)] =
    fieldList
      .fieldValues(this)
      .collect:
        case (fieldName, value: T) => (fieldName, value)
  end valuesByType
end ReflectiveEnumeration

object ReflectiveEnumeration:
  trait Enumerable

  trait FieldList[R <: ReflectiveEnumeration]:
    def fieldValues(r: R): IArray[(String, Enumerable)]
  end FieldList

  inline given inferredFieldList: [R <: ReflectiveEnumeration] => FieldList[R] =
    ${ inferredFieldListImpl[R] }

  private[forja] def inferredFieldListImpl[R <: ReflectiveEnumeration: Type](
      using Quotes,
  ): Expr[FieldList[R]] =
    import quotes.reflect.*
    '{
      new FieldList[R]:
        def fieldValues(r: R): IArray[(String, Enumerable)] =
          ${
            val tp = TypeRepr.of[R]
            if !tp.isSingleton
            then
              report.errorAndAbort(
                s"${tp.show} must be a singleton",
                Symbol.spliceOwner.pos.get,
              )

            val syms =
              (tp.typeSymbol.methodMembers ++ tp.typeSymbol.fieldMembers)
                .filter: m =>
                  tp.select(m) <:< TypeRepr.of[Enumerable]
                .filterNot(_.flags.is(Flags.Protected | Flags.Private))
                .sortBy: m =>
                  m.pos
                    .map: pos =>
                      (pos.sourceFile.name, pos.start, pos.end)
                    .getOrElse(("", 0, 0))

            val exprs = syms.map: sym =>
              '{
                (
                  ${ Expr(sym.name) },
                  ${ 'r.asTerm.select(sym).asExprOf[Enumerable] },
                )
              }
            '{ IArray(${ Varargs(exprs) }*) }
          }
        end fieldValues
    }
  end inferredFieldListImpl
end ReflectiveEnumeration
