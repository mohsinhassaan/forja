package forja

import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.quotes
import scala.quoted.Expr

into opaque type P[T] = P.Erased

object P:
  trait Meta[T]:
    def erase(t: T): Erased
  end Meta

  object Meta:
    inline def derived[T]: Meta[T] = ${ derivedImpl }

    private def derivedImpl[T : Type](using Quotes): Expr[Meta[T]] =
      import quotes.reflect.*

      val tp = TypeRepr.of[T]
      val classSym = tp.classSymbol.get
      if classSym.flags.is(Flags.Sealed) && classSym.flags.is(Flags.Abstract)
      then
        // TODO: the derives clause only applies to the type on which it is declared.
        // So, if you say a whole enum derives P.Meta, we get one instance for the overall enum type.
        // Two interesting questions:
        // 1. We are Meta[T] and we got given U <: T; do we have to do runtime type dispatch every time?
        // 2. How to accept being truly given a supertype, so passing T above also works.
        ???
      else if classSym.flags.is(Flags.Case)
      then
        val instSym = Symbol.newMethod(
          Symbol.spliceOwner,
          "inst",
          MethodType(
            classSym.caseFields.map(fld => s"fld$$${fld.name}")
          )(
            { _ =>
              classSym.caseFields.map(fld => fld.info)
            }, { _ =>
              TypeRepr.of[Erased]
            },
          ),
        )

        Block(
          List(
            DefDef(
              instSym,
              {
                case List(instArgs) =>
                  val freshCls = Symbol.newClass(
                    owner = instSym,
                    name = s"Erased${classSym.name}",
                    parents = List(TypeRepr.of[Object], TypeRepr.of[Erased]),
                    decls = sym => {
                      List(
                        Symbol.newMethod(sym, "rewriteInner", MethodType(List("fn"))(_ => List(TypeRepr.of[Erased => Erased]), _ => TypeRepr.of[Erased])),
                      ) :::
                      classSym.caseFields.map: fld =>
                        Symbol.newVal(
                          sym,
                          s"fld$$${fld.name}",
                          fld.info,
                          Flags.EmptyFlags,
                          Symbol.noSymbol,
                        )
                    },
                    selfType = None,
                  )
                  Some {
                    Block(
                      List(
                        ClassDef(
                          cls = freshCls,
                          parents = List(TypeTree.of[Object], TypeTree.of[Erased]),
                          body = List(
                            DefDef(freshCls.methodMember("rewriteInner").head, {
                              case List(List(fn)) =>
                                val sym = freshCls.methodMember("rewriteInner").head
                                given Quotes = sym.asQuotes
                                Some:
                                  ValDef.let(
                                    sym,
                                    freshCls.declaredFields.map { fld =>
                                      fld.info.asType match
                                        case '[ft] =>
                                          Expr.summon[RewriteInner[ft]] match
                                            case Some(rwInner) =>
                                              '{
                                                $rwInner.rewriteInner(
                                                  ${ This(freshCls).select(fld).asExprOf[ft] },
                                                  ${fn.asExprOf[Erased => Erased]},
                                                )
                                              }.asTerm
                                            case None =>
                                              report.errorAndAbort(s"no rewrite rule for ${TypeRepr.of[ft].show}")
                                          end match
                                      end match
                                    },
                                  ) { binds =>
                                    val didChangeExpr = freshCls.declaredFields
                                      .zip(binds)
                                      .map: (fld, bind) =>
                                        '{ ${This(freshCls).select(fld).asExpr}.asInstanceOf[AnyRef] ne ${bind.asExpr}.asInstanceOf[AnyRef] }
                                      .foldLeft('{ false })((l, r) => '{ $l || $r })
                                    end didChangeExpr
                                    '{
                                      val didChange = $didChangeExpr
                                      if didChange
                                      then ${
                                        Ref(instSym)
                                          .appliedToArgs(freshCls.declaredFields.map(This(freshCls).select))
                                          .asExprOf[Erased]
                                      }
                                      else ${This(freshCls).asExprOf[Erased]}
                                    }
                                    .asTerm
                                  }
                              case _ => ???
                            })
                          ) ::: freshCls.declaredFields.zip(instArgs).map: (fld, instArg) =>
                            ValDef.apply(fld, Some(instArg.asExpr.asTerm)),
                        )
                      ),
                      New(TypeTree.ref(freshCls))
                        .select(freshCls.primaryConstructor)
                        .appliedToArgs(Nil),
                    )
                  }
                case _ => ???
              },
            ),
          ),
          '{
            new Meta[T]:
              def erase(t: T): Erased = ${
                Ref(instSym)
                  .appliedToArgs:
                    classSym.caseFields
                      .map: fld =>
                        '{ t }.asTerm.select(fld)
                  .asExprOf[Erased]
              }
            end new
          }
          .asTerm
        )
        .asExprOf[Meta[T]]
      else
        report.errorAndAbort(s"${tp.show} is neither a case class nor a sealed abstract type")
      end if
    end derivedImpl
  end Meta

  given [T] => (meta: Meta[T]) => Conversion[T, P[T]]:
    def apply(x: T): P[T] = meta.erase(x)
  end given

  trait Erased:
    def rewriteInner(fn: Erased => Erased): Erased
  end Erased

  extension [T] (p: P[T])
    def fixpoint(fn: [U] => P[U] => P[U]): P[T] = ???
  end extension

  trait RewriteInner[T]:
    def rewriteInner(t: T, fn: Erased => Erased): T
  end RewriteInner

  object RewriteInner:
    given RewriteInner[Byte]:
      inline def rewriteInner(t: Byte, fn: Erased => Erased): Byte = t
    end given

    given RewriteInner[Char]:
      inline def rewriteInner(t: Char, fn: Erased => Erased): Char = t
    end given

    given RewriteInner[Short]:
      inline def rewriteInner(t: Short, fn: Erased => Erased): Short = t
    end given

    given RewriteInner[Int]:
      inline def rewriteInner(t: Int, fn: Erased => Erased): Int = t
    end given

    given RewriteInner[Long]:
      inline def rewriteInner(t: Long, fn: Erased => Erased): Long = t
    end given

    given RewriteInner[Float]:
      inline def rewriteInner(t: Float, fn: Erased => Erased): Float = t
    end given

    given RewriteInner[Double]:
      inline def rewriteInner(t: Double, fn: Erased => Erased): Double = t
    end given

    given RewriteInner[String]:
      inline def rewriteInner(t: String, fn: Erased => Erased): String = t
    end given

    given [T] => RewriteInner[P[T]]:
      inline def rewriteInner(t: P[T], fn: Erased => Erased): Erased = t.rewriteInner(fn)
    end given

    given [T <: AnyRef] => (rewriteElem: RewriteInner[T]) => RewriteInner[List[T]]:
      def rewriteInner(t: List[T], fn: Erased => Erased): List[T] =
        t.mapConserve(rewriteElem.rewriteInner(_, fn))
      end rewriteInner
    end given
  end RewriteInner
end P
