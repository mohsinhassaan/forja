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
        val erasedSym = Symbol.newClass(
          owner = Symbol.spliceOwner,
          name = s"Erased${classSym.name}",
          parents = _ => List(TypeRepr.of[Object], TypeRepr.of[Erased]),
          decls = sym => {
            List(
              Symbol.newMethod(sym, "rewriteInner", MethodType(List("fn"))(_ => List(TypeRepr.of[Erased => Erased]), _ => TypeRepr.of[Erased])),
            )
          },
          selfType = None,
          clsFlags = Flags.EmptyFlags,
          clsPrivateWithin = Symbol.noSymbol,
          clsAnnotations = Nil,
          conMethodType = { resultTpe =>
            MethodType(classSym.caseFields.map(fld => s"fld$$${fld.name}"))(
              _ => classSym.caseFields.map(_.info),
              _ => resultTpe,
            )
          },
          conFlags = Flags.EmptyFlags,
          conPrivateWithin = Symbol.noSymbol,
          conParamFlags = List(classSym.caseFields.map(_ => Flags.ParamAccessor)),
          conParamPrivateWithins = List(classSym.caseFields.map(_ => Symbol.noSymbol)),
        )
        val metaSym = Symbol.newClass(
          Symbol.spliceOwner,
          s"Meta${classSym.name}",
          List(TypeRepr.of[Object], TypeRepr.of[Meta[T]]),
          { sym =>
            List(
              Symbol.newMethod(
                sym,
                "erase",
                MethodType(List("t"))(
                  { sym => List(TypeRepr.of[T]) },
                  { sym => TypeRepr.of[Erased] },
                ),
                Flags.Inline & Flags.Method,
                Symbol.noSymbol,
              ),
            )
          },
          None,
        )

        Block(
          List(
            ClassDef(
              cls = erasedSym,
              parents = List(TypeTree.of[Object], TypeTree.of[Erased]),
              body = List(
                DefDef(erasedSym.declaredMethod("rewriteInner").head, {
                  case List(List(fn)) =>
                    val sym = erasedSym.methodMember("rewriteInner").head
                    given Quotes = sym.asQuotes
                    Some:
                      ValDef.let(
                        sym,
                        erasedSym.declaredFields.map { fld =>
                          fld.info.asType match
                            case '[ft] =>
                              Expr.summon[RewriteInner[ft]] match
                                case Some(rwInner) =>
                                  '{
                                    $rwInner.rewriteInner(
                                      ${ This(erasedSym).select(fld).asExprOf[ft] },
                                      ${ fn.asExprOf[Erased => Erased] },
                                    )
                                  }.asTerm
                                case None =>
                                  report.errorAndAbort(s"no rewrite rule for ${TypeRepr.of[ft].show}")
                              end match
                          end match
                        },
                      ) { binds =>
                        val didChangeExpr = erasedSym.declaredFields
                          .zip(binds)
                          .map: (fld, bind) =>
                            This(erasedSym).select(fld).asExpr match
                              case '{ $nv: AnyRef } =>
                                '{ $nv ne ${ bind.asExpr }.asInstanceOf[AnyRef] }
                              case '{ $nv: nvT } =>
                                '{ $nv != ${ bind.asExpr} }
                            end match
                          .foldLeft('{ false })((l, r) => '{ $l || $r })
                        end didChangeExpr
                        '{
                          val didChange = $didChangeExpr
                          if didChange
                          then ${
                            New(TypeIdent(erasedSym))
                              .select(erasedSym.primaryConstructor)
                              .appliedToArgs(erasedSym.declaredFields.map(This(erasedSym).select))
                              .asExprOf[Erased]
                          }
                          else ${This(erasedSym).asExprOf[Erased]}
                        }
                        .asTerm
                      }
                  case _ => ???
                })
              )
            ),
            ClassDef(
              metaSym,
              List(TypeTree.of[Object], TypeTree.of[Meta[T]]),
              List(
                DefDef(
                  metaSym.declaredMethod("erase").head,
                  {
                    case List(List(t)) =>
                      Some:
                        New(TypeIdent(erasedSym))
                          .select(erasedSym.primaryConstructor)
                          .appliedToArgs:
                            classSym.caseFields
                              .map: fld =>
                                t
                                  .asExpr
                                  .asTerm
                                  .select(fld)
                    case _ => ???
                  },
                )
              ),
            )
          ),
          New(TypeIdent(metaSym))
            .select(metaSym.primaryConstructor)
            .appliedToArgs(Nil)
        )
        .asExprOf[Meta[T]]
      else
        report.errorAndAbort(s"${tp.show} is neither a case class nor a sealed abstract type")
      end if
    end derivedImpl
  end Meta

  given [T, U <: T] => (meta: Meta[T]) => Conversion[U, P[T]]:
    def apply(x: U): P[T] = meta.erase(x)
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
