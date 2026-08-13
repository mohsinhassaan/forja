package forja

import scala.quoted.Type
import scala.quoted.Quotes
import scala.quoted.Expr
import scala.annotation.publicInBinary
import scala.quoted.quotes
import forja.util.TupleMacros.*
import forja.Lang2.SumMeta
import forja.Lang2.ErasedNode
import forja.util.ExprMacros.summonOrAbort
import scala.language.experimental.erasedDefinitions
import scala.annotation.tailrec

@publicInBinary
private[forja] object MetaMacros:
  def findCompanion[T: Type](using Quotes)(): Expr[Any] =
    import quotes.reflect.*
    val clsSym = TypeRepr.of[T].classSymbol.get
    Ident(clsSym.companionModule.termRef).asExpr
    // clsSym.companionModule.termRef.asType match
    //   case '[c] =>
    //     '{
    //       ${ Expr.summonOrAbort[ValueOf[c]] }
    //         .value
    //     }
    // end match
  end findCompanion

  def findTermCasesRaw[T : Type](using Quotes)(): List[(String, Type[?])] =
    import quotes.reflect.*

    val tRepr = TypeRepr.of[T]
    tRepr
      .typeSymbol
      .methodMembers
      .flatMap: meth =>
        meth.typeRef.asType match
          case '[F[t]] =>
            List((meth.name, Type.of[t]))
          case _ => Nil
        end match
  end findTermCasesRaw

  def termDerivedImpl[T : Type](using Quotes): Expr[Term[T]] =
    import quotes.reflect.*

    val tRepr = TypeRepr.of[T]
    tRepr.classSymbol match
      case None =>
        report.errorAndAbort(s"${tRepr.show} does not refer to a class")
      case Some(clsSym) =>
        val termCasesRaw = findTermCasesRaw[T]()
        val termCasesF = termCasesRaw.map: (name, tp) =>
          tp match
            case '[tp] => (name, TypeRepr.of[F[tp]])
          end match
        end termCasesF
        val termCasesErased = termCasesRaw.map: (name, tp) =>
          tp match
            case '[tp] =>
              Expr.summonOrAbort[TypeAdjust.Erased[tp]] match
                case '{ $_ : TypeAdjust.Erased.Aux[?, tpe] } =>
                  (Symbol.freshName(name), TypeRepr.of[tpe])
          end match
        end termCasesErased

        val dataName = Symbol.freshName("data")
        val dataParents = List(TypeTree.of[T], TypeTree.of[Lang2.ErasedNode])
        val dataClsSym = Symbol.newClass(
          owner = Symbol.spliceOwner,
          name = clsSym.name,
          parents = _ => dataParents.map(_.tpe),
          decls = { selfSym =>
            termCasesF.map { (name, tpe) =>
              Symbol.newMethod(
                parent = selfSym,
                name = name,
                tpe = ByNameType(tpe),
              )
            }
            ++ List(
              Symbol.newMethod(
                parent = selfSym,
                name = "toString",
                tpe = MethodType(MethodTypeKind.Plain)(Nil)(_ => Nil, _ => TypeRepr.of[String]),
                flags = Flags.Override,
                privateWithin = Symbol.noSymbol,
              ),
            )
          },
          selfType = None,
          clsFlags = Flags.Final,
          clsPrivateWithin = Symbol.noSymbol,
          conParams = termCasesErased,
        )
        val dataClsDecl = ClassDef(
          cls = dataClsSym,
          parents = dataParents,
          body = termCasesRaw.zip(termCasesErased).map {
            case ((name, tpe), (erasedName, erasedTpe)) =>
              val erasedSym = dataClsSym.fieldMember(erasedName)
              val methSym = dataClsSym.methodMember(name).head
              given Quotes = methSym.asQuotes
              tpe match
                case '[tpe] =>
                  DefDef(
                    methSym,
                    { paramss =>
                      assert(paramss.isEmpty)
                      Some:
                        '{ F[tpe](${ This(dataClsSym).select(erasedSym).asExpr }) }.asTerm
                    },
                  )
              end match
          } ++ List(
            DefDef(
              dataClsSym.methodMember("toString").head,
              { _ =>
                val owner = dataClsSym.methodMember("toString").head
                given Quotes = owner.asQuotes
                Some:
                  '{
                    val builder = StringBuilder()
                    builder ++= ${ Expr(clsSym.name) }
                    builder += '('
                    ${
                      Expr.ofList:
                        termCasesErased
                          .map: (name, tpe) =>
                            This(dataClsSym)
                              .select(dataClsSym.fieldMember(name))
                              .asExpr
                    }.addString(builder, ", ")
                    builder += ')'
                    builder.result()
                  }
                  .asTerm
              },
            )
          ),
        )
        Block(
          List(
            dataClsDecl,
          ),
          '{
            forja.Term:
              new Lang2.TermMeta[T]:
                def Companion: Companion =
                  ${ findCompanion[T]() }.asInstanceOf[Companion]
                end Companion

                @publicInBinary
                def applyImpl(cases: Cases): T =
                  ${
                    val tpl = defn.TupleClass(termCasesErased.size) match
                      case tpl if tpl.isNoSymbol => TypeRepr.of[Tuple].typeSymbol.typeRef
                      case tpl => tpl.typeRef.appliedTo(termCasesErased.map(_._2))
                    end tpl
                    tpl.asType match
                      case '[type tpl <: Tuple; tpl] =>
                        '{
                          val erasedCases = cases.asInstanceOf[tpl]
                          ${
                            val tplSym = tpl.typeSymbol
                            // report.errorAndAbort(tplSym.fieldMembers.map(_.name).mkString(", "))
                            Apply(
                              Select(New(TypeIdent(dataClsSym)), dataClsSym.primaryConstructor),
                              termCasesErased
                                .map(_._2)
                                .zipWithIndex
                                .map { (tpe, i) =>
                                  tpe.asType match
                                    case '[tpe] =>
                                      tplSym.fieldMember(s"_$i") match
                                        case accessor if !accessor.isNoSymbol =>
                                          '{ ${ '{ erasedCases }.asTerm.select(accessor).asExpr }.asInstanceOf[tpe] }.asTerm
                                        case _ =>
                                          '{ erasedCases.productElement(${ Expr(i) }).asInstanceOf[tpe] }.asTerm
                                      end match
                                  end match
                                }
                                .toList,
                            )
                              .asExprOf[T]
                          }
                        }
                    end match
                  }
                end applyImpl
              end new
          }
          .asTerm,
        )
        .asExprOf[forja.Term[T]]
    end match
  end termDerivedImpl

  def termMetaImpl[L <: Lang2 : Type, T : Type](term: Expr[Term[T]])(using Quotes): Expr[Lang2.TermMeta[T]] =
    ???
  end termMetaImpl

  def findSumCasesRaw[T : Type](using Quotes)(): List[(Type[?], Int)] =
    import quotes.reflect.*
    val clsSym = TypeRepr.of[T].classSymbol.get
    val companionSym = clsSym.companionModule
    val memberSyms = companionSym
      .typeMembers
      .filter: elemCandidate =>
        elemCandidate.typeRef.asType match
          case '[ec] =>
            Expr.summon[forja.Term[ec] | Sum[ec]] match
              case Some(_) => true
              case None => false
            end match
        end match
      .sortBy(_.name)
    end memberSyms
    memberSyms
      .map(_.typeRef.asType)
      .zipWithIndex
  end findSumCasesRaw

  def sumDerivedImpl[T : Type](using Quotes): Expr[Sum[T]] =
    import quotes.reflect.*

    val tRepr = TypeRepr.of[T]
    tRepr.classSymbol match
      case None =>
        report.errorAndAbort(s"${tRepr.show} does not refer to a class")
      case Some(clsSym) =>
        findSumCasesRaw[T]() // smoke test
        val ordinalName = Symbol.freshName("ordinal")
        val elemName = Symbol.freshName("elem")
        val dataParents = List(TypeTree.of[T], TypeTree.of[Lang2.ErasedNode])
        val dataClsSym = Symbol.newClass(
          owner = Symbol.spliceOwner,
          name = clsSym.name,
          parents = _ => dataParents.map(_.tpe),
          decls = selfSym =>
            Nil,
          selfType = None,
          clsFlags = Flags.Final,
          clsPrivateWithin = Symbol.noSymbol,
          conParams = List(
            ordinalName -> TypeRepr.of[Int],
            elemName -> TypeRepr.of[Lang2.ErasedNode],
          ),
        )
        val ordinalSym = dataClsSym.declaredField(ordinalName)
        assert(!ordinalSym.isNoSymbol)
        val dataClsDecl = ClassDef(
          cls = dataClsSym,
          parents = dataParents,
          body = Nil,
        )
        Block(
          List(
            dataClsDecl,
          ),
          '{
            forja.Sum:
              new Lang2.SumMeta[T]:
                @publicInBinary
                def applyImpl(idx: Int, elem: ErasedNode): T =
                  ${
                    Apply(
                      Select(New(TypeIdent(dataClsSym)), dataClsSym.primaryConstructor),
                      List('{ idx }.asTerm, '{ elem }.asTerm),
                    )
                    .asExprOf[T]
                  }
                end applyImpl

                def Companion: Companion =
                  ${ findCompanion[T]() }.asInstanceOf[Companion]
                end Companion

                extension (Companion: Companion)
                  def ordinal(t: T): Int =
                    ${
                      TypeIdent(dataClsSym).tpe.asType match
                        case'[dataCls] =>
                          '{ t.asInstanceOf[dataCls] }
                            .asTerm
                            .select(ordinalSym)
                            .asExprOf[Int]
                    }
                  end ordinal
                end extension
              end new
          }
          .asTerm,
        )
        .asExprOf[forja.Sum[T]]
    end match
  end sumDerivedImpl

  def sumMetaImpl[M <: Lang2 : Type, T : Type](sum: Expr[Sum[T]])(using Quotes): Expr[Lang2.SumMeta[T]] =
    import quotes.reflect.*

    // report.errorAndAbort(TypeRepr.of[M].classSymbol.get.termRef.show(using Printer.TypeReprStructure))

    TypeRepr.of[M].asType match
      case '[type l <: Lang2; l] =>
        val companion = findCompanion[T]()
        val companionTpe = companion.asTerm.tpe

        val casesRaw = findSumCasesRaw[T]()
        val cases = casesRaw
          .map(_._1)
          .map:
            case '[cs] =>
              Expr.summonOrAbort[TypeAdjust.Effective[l, cs]] match
                case '{ $_ : TypeAdjust.Effective.Aux[?, ?, cs2] } =>
                  TypeRepr.of[cs2]
              end match
        end cases

        report.errorAndAbort(cases.map(_.show(using Printer.TypeReprStructure)).toString)

        (companionTpe.asType, cases.map(_.asType).toTupleType).runtimeChecked match
          case ('[companion], '[type cases <: Tuple; cases]) =>
            '{ ${ sum }.meta.asInstanceOf[SumMeta[T] {
              type Companion = companion
              type Cases = cases
            }] }
        end match
    end match
  end sumMetaImpl

  def sumApplyImpl[Cases <: Tuple : Type, T : Type](using Quotes)(meta: Expr[SumMeta[T]], elem: Expr[Tuple.Union[Cases]]): Expr[T] =
    elem match
      case '{ type t <: Tuple.Union[Cases]; $elem : t } =>
        val idx = Type.of[Cases]
          .toList
          .indexWhere:
            case '[`t`] => true
            case _ => false
        end idx
        '{
          val boundMeta = ${ meta }
          ${
            if idx == -1
            then
              import quotes.reflect.*
              val caseDefs = Type.of[Cases]
                .toList
                .zipWithIndex
                .map: p =>
                  p.runtimeChecked match
                    case ('[cs], idx) =>
                      CaseDef(
                        pattern = Typed(Wildcard(), TypeTree.of[cs]),
                        guard = None,
                        rhs = Literal(IntConstant(idx)),
                      )
                  end match
              end caseDefs

              '{
                boundMeta.applyImpl(
                  ${
                    Match(
                      selector = elem.asTerm,
                      cases = caseDefs,
                    )
                    .asExprOf[Int]
                  },
                  ${ elem }.asInstanceOf[ErasedNode],
                )
              }
            else '{ boundMeta.applyImpl(${ Expr(idx) }, ${ elem }.asInstanceOf[Lang2.ErasedNode]) }
          }
        }
    end match
  end sumApplyImpl

  def isNodeImpl[N : Type](using Quotes): Expr[Lang2.IsNode.Aux[N, ?]] =
    import quotes.reflect.*

    val langRepr = TypeRepr.of[Lang2]

    @tailrec
    def impl(tp: TypeRepr): Type[? <: Lang2] =
      println("isNode " + tp.show(using Printer.TypeReprStructure))
      tp match
        case _ if tp <:< langRepr =>
          tp.asType match
            case '[type tp <: Lang2; tp] =>
              Type.of[tp]
          end match
        case TypeRef(base, _) =>
          impl(base)
        case TermRef(base, _) =>
          impl(base)
        case ThisType(inner) =>
          impl(inner)
        case _ =>
          report.errorAndAbort(s"Unsupported: ${tp.show}, starting from ${TypeRepr.of[N].show}")
      end match
    end impl

    impl(TypeRepr.of[N]) match
      case '[type l <: Lang2; l] =>
        '{ new Lang2.IsNode.Aux[N, l] }
    end match
  end isNodeImpl

  def launderNodeImpl[L1 <: Lang2 : Type, L2 <: Lang2 : Type, N1 : Type](using Quotes): Expr[Lang2.LaunderNode.Aux[L2, N1, ?]] =
    import quotes.reflect.*

    def impl(tp: TypeRepr): TypeRepr =
      println("launder " + tp.show(using Printer.TypeReprStructure))
      tp match
        case _ if tp <:< TypeRepr.of[L1] =>
          TypeRepr.of[L2]
        case TypeRef(base, name) =>
          val rec = impl(base)
          val member = rec.typeSymbol.typeMember(name.stripSuffix("$"))
          if member.isNoSymbol
          then
            println(s"Could not make type ref $name in ${rec.show}, goal is ${TypeRepr.of[N1].show}")
            report.errorAndAbort(s"Could not make type ref $name in ${rec.show}, goal is ${TypeRepr.of[N1].show}")
          rec.select(member)
        case TermRef(base, name) =>
          val rec = impl(base)
          val member = rec.termSymbol.fieldMember(name.stripSuffix("$"))
          if member.isNoSymbol
          then
            println(s"Could not make term ref $name in ${rec.show}, goal is ${TypeRepr.of[N1].show}")
            report.errorAndAbort(s"Could not make term ref $name in ${rec.show}, goal is ${TypeRepr.of[N1].show}")
          rec.select(member)
        case ThisType(inner) =>
          impl(inner)
        case _ =>
          report.errorAndAbort(s"Unsupported: ${tp.show}, starting from ${TypeRepr.of[N1].show}")
      end match
    end impl

    impl(TypeRepr.of[N1]).asType match
      case '[n2] =>
        '{ new Lang2.LaunderNode.Aux[L2, N1, n2] }
    end match
  end launderNodeImpl

  def hasSameMetaImpl[N1 : Type, N2 : Type, L1 <: Lang2 : Type, L2 <: Lang2 : Type](using Quotes): Expr[Lang2.HasSameMeta[N1, N2]] =
    import quotes.reflect.*

    val l1 = TypeRepr.of[L1]
    val l2 = TypeRepr.of[L2]
    val commonBases = l1
      .baseClasses
      .intersect(l2.baseClasses)
      .filter(_.typeRef <:< TypeRepr.of[Lang2])
      .filterNot(_.typeRef =:= TypeRepr.of[Lang2])
    end commonBases

    if commonBases.isEmpty
    then report.errorAndAbort(s"${ TypeRepr.of[L1].show } and ${ TypeRepr.of[L2].show } have only ${ TypeRepr.of[Lang2].show } as shared base trait")

    if commonBases.size > 2
    then report.errorAndAbort(s"${ TypeRepr.of[L1].show } and ${ TypeRepr.of[L2].show } share multiple Lang2 base traits: ${ commonBases.map(_.typeRef.show).mkString(", ") }")

    '{ new Lang2.HasSameMeta[N1, N2] }
  end hasSameMetaImpl
end MetaMacros
