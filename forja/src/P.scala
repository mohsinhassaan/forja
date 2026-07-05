package forja

import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.quotes
import scala.quoted.Expr
import scala.util.NotGiven
import scala.quoted.Varargs
import scala.annotation.implicitNotFound
import scala.compiletime.summonInline

into opaque type P[T] = P.Erased

object P:
  @implicitNotFound("${T} or its sealed supertype needs to derive P.Meta")
  trait Meta[T]:
    def erase(t: T): Erased
    // Gotcha: this could more sensibly convert to P[T], but some interaction of the opaque type
    // being in scope here and the macro definitions below breaks things. The error looks like
    // the macro hardcodes Erased where I lexically wrote P[T], causing the compiler to notice later
    // on that def conversion: Conversion[T, Erased] would not implement the P[T] version.
    // I guess this is because the macro is expanded in another file...
    // Instead, we use Conversion's variance to replace Erased with P[T] during the given that calls
    // this method.
    def conversion: Conversion[T, Erased]
  end Meta

  object Meta:
    inline def derived[T]: Meta[T] = ${ derivedImpl }

    private def derivedImpl[T : Type](using Quotes): Expr[Meta[T]] =
      import quotes.reflect.*

      val tpTree = TypeTree.of[T]
      println(s"process: ${tpTree.show}")
      if tpTree.symbol.flags.is(Flags.Sealed) && tpTree.symbol.flags.is(Flags.Abstract)
      then
        println(s"abstract: ${tpTree.show}")
        '{
          final class GeneratedMeta extends Conversion[T, Erased], Meta[T]:
            // TODO: array is simpler, but if it's ever a bottleneck then we can generate a synthetic fieldset
            private val subMetas: Array[Meta[?]] =
              ${
                val childMetas = tpTree.symbol.children.map: childSym =>
                  childSym.typeRef.asType match
                    case '[ch] => '{ derived[ch] }
                end childMetas
                '{ Array(${Varargs(childMetas)}*) }
              }
            end subMetas

            def erase(t: T): Erased =
              val subMetasProxy = subMetas
              ${
                Match(
                  '{t}.asTerm,
                  tpTree.symbol.children.zipWithIndex.map { (childSym, idx) =>
                    def branchBody =
                      '{ subMetasProxy(${Expr(idx)}).asInstanceOf[Meta[T]].erase(t) }.asTerm
                    end branchBody
                    if childSym.isTerm
                    then
                      CaseDef(
                        Ref(childSym),
                        None,
                        branchBody,
                      )
                    else if childSym.isType
                    then
                      TypeTree.ref(childSym).tpe.asType match
                        case '[cT] =>
                          CaseDef(
                            Typed(Wildcard(), TypeTree.of[cT]),
                            None,
                            branchBody,
                          )
                    else
                      report.errorAndAbort(s"neither a term nor a type $childSym")
                    end if
                  },
                )
                  .asExprOf[Erased]
              }
            end erase
            
            def conversion: Conversion[T, Erased] = this

            def apply(t: T): Erased = erase(t)
          end GeneratedMeta

          new GeneratedMeta
        }
      else if tpTree.symbol.flags.is(Flags.Case)
      then
        val erasedSym = Symbol.newClass(
          owner = Symbol.spliceOwner,
          name = s"Erased${tpTree.symbol.name}",
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
            MethodType(tpTree.symbol.caseFields.map(fld => s"fld$$${fld.name}"))(
              _ => tpTree.symbol.caseFields.map(fld => Ref(fld).tpe.widen),
              _ => resultTpe,
            )
          },
          conFlags = Flags.EmptyFlags,
          conPrivateWithin = Symbol.noSymbol,
          conParamFlags = List(tpTree.symbol.caseFields.map(_ => Flags.ParamAccessor)),
          conParamPrivateWithins = List(tpTree.symbol.caseFields.map(_ => Symbol.noSymbol)),
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
                        erasedSym.declaredFields
                          .zip(tpTree.symbol.caseFields)
                          .map { (fld, origFld) =>
                            Ref(fld).tpe.widen.asType match
                              case '[ft] =>
                                Implicits.search(TypeRepr.of[RewriteInner[ft]]) match
                                  case success: ImplicitSearchSuccess =>
                                    '{
                                      ${ success.tree.asExprOf[RewriteInner[ft]] }.rewriteInner(
                                        ${ This(erasedSym).select(fld).asExprOf[ft] },
                                        ${ fn.asExprOf[Erased => Erased] },
                                      )
                                    }.asTerm
                                  case failure: ImplicitSearchFailure =>
                                    report.errorAndAbort(
                                      failure.explanation,
                                      origFld.pos.getOrElse(Position.ofMacroExpansion),
                                    )
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
                  case _ => throw RuntimeException("unreachable")
                })
              )
            ),
          ),
          '{
            final class GeneratedMeta extends Conversion[T, Erased], Meta[T]:
              def erase(t: T): Erased =
                ${
                  New(TypeIdent(erasedSym))
                    .select(erasedSym.primaryConstructor)
                    .appliedToArgs:
                      tpTree.symbol.caseFields
                        .map: fld =>
                          '{t}
                            .asTerm
                            .select(fld)
                    .asExprOf[Erased]
                }
              end erase

              def conversion: Conversion[T, Erased] = this

              def apply(t: T): Erased = erase(t)
            end GeneratedMeta

            new GeneratedMeta
          }
          .asTerm,
        )
        .asExprOf[Meta[T]]
      else
        report.errorAndAbort(s"${tpTree.show} is neither a case class nor a sealed abstract type")
      end if
    end derivedImpl
  end Meta

  // Implicit lookup on traits with variance can be glitchy, so this manually implements the
  // "subtyping" rule that a Meta[U <: T] can be implemented via a Meta[T]. It is just redundant that
  // the first operation in erase(u) will therefore be to check that u instanceof U.
  inline given [T, U <: T] => (meta: Meta[T]) => (=>NotGiven[U =:= T]) => Meta[U] = meta.asInstanceOf

  // This awkward pattern lets us customize the implicit not found message.
  // Technically the Conversion is considered to "succeed", even if all it does is immediately
  // fail summonInline, therefore showing the failure for looking up Meta[T], not Conversion[T, P[T]].
  inline given [T] => Conversion[T, P[T]] = summonInline[Meta[T]].conversion

  trait Erased:
    def rewriteInner(fn: Erased => Erased): Erased
  end Erased

  extension [T] (p: P[T])
    def fixpoint(fn: [U] => P[U] => P[U]): P[T] = ???
  end extension

  @implicitNotFound("no rule found to rewrite P[?] inside ${T}")
  trait RewriteInner[T]:
    def rewriteInner(t: T, fn: Erased => Erased): T
  end RewriteInner

  object RewriteInner:
    given RewriteInner[Boolean]:
      inline def rewriteInner(t: Boolean, fn: Erased => Erased): Boolean = t
    end given

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
