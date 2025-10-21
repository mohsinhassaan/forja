package forja

import cats.data.Chain
import cats.{Alternative, Eval, Foldable}

import forja.util.ReflectiveEnumeration

import scala.collection.mutable
import scala.reflect.TypeTest

import Query.*

sealed trait Query[+T]:
  query =>
  private[Query] final def altOptions: Chain[Query[T]] =
    query match
      case query: alt[u] =>
        query.left.altOptions ++ query.right.altOptions
      case _ => Chain.one(query)
  end altOptions

  final infix def |[U >: T](other: Query[U]): Query[U] = new alt(query, other)

  final def map[U](fn: T => U): Query[U] = new map(query, fn)

  final def cast[TT >: T, U](using U <:< TT): Query[U] = query.asInstanceOf

  final def here[TT >: T, U](at: Query[U])(using ev: TT <:< Node): Query[U] =
    new here(query.cast(using ev), at)

  final def runQuery(node: Node): Eval[Option[T]] =
    runQueryImpl(node)

  protected def runQueryImpl(node: Node): Eval[Option[T]]
end Query

object Query:
  given alternative: Alternative[Query]:
    def ap[A, B](ff: Query[A => B])(fa: Query[A]): Query[B] = new ap(ff, fa)
    def combineK[A](x: Query[A], y: Query[A]): Query[A] = x | y
    def empty[A]: Query[A] = Query.empty
    def pure[A](x: A): Query[A] = Query.pure(x)
  end alternative

  object empty extends Query[Nothing]:
    protected def runQueryImpl(node: Node): Eval[Option[Nothing]] =
      Eval.now(None)
    end runQueryImpl
  end empty

  final class pure[T](value: T) extends Query[T]:
    protected def runQueryImpl(node: Node): Eval[Option[T]] =
      Eval.now(Some(value))
    end runQueryImpl
  end pure

  final class on[T](patternFn: PatternContext ?=> Pattern[T]) extends Query[T]:
    val pattern = patternFn(using PatternContext)

    protected def runQueryImpl(node: Node): Eval[Option[T]] =
      pattern.runPattern(node.emptyNodeSpanHere).map(_.map(_._1))
    end runQueryImpl

    final class rewrite(fn: T => Node | Iterable[Node])
        extends ReflectiveEnumeration.Enumerable:
      val pattern = on.this.pattern.rewrite(fn)
    end rewrite
  end on

  private final class map[T, U](val query: Query[T], fn: T => U)
      extends Query[U]:
    protected def runQueryImpl(node: Node): Eval[Option[U]] =
      Eval.defer(query.runQuery(node)).map(_.map(fn))
    end runQueryImpl
  end map

  private final class ap[A, B](val ff: Query[A => B], val fa: Query[A])
      extends Query[B]:
    protected def runQueryImpl(node: Node): Eval[Option[B]] =
      Eval
        .defer(ff.runQuery(node))
        .flatMap:
          case None     => Eval.now(None)
          case Some(ff) =>
            Eval.defer(fa.runQuery(node)).map(_.map(ff))
    end runQueryImpl
  end ap

  private final class here[T](val place: Query[Node], val query: Query[T])
      extends Query[T]:
    protected def runQueryImpl(node: Node): Eval[Option[T]] =
      place
        .runQuery(node)
        .flatMap:
          case Some(node) =>
            query.runQuery(node)
          case None => Eval.now(None)
    end runQueryImpl
  end here

  private final class alt[T](val left: Query[T], val right: Query[T])
      extends Query[T]:
    import DecisionTree.*
    private lazy val decisionTree: FrozenRecordList[T, Query] =
      DecisionTree.fromQuery(this)

    protected def runQueryImpl(node: Node): Eval[Option[T]] =
      decisionTree.run(node.emptyNodeSpanHere, _.runQuery(node))
    end runQueryImpl
  end alt

  private[forja] object DecisionTree:
    private def scanPattern[T, Q[_] <: Query[?] | Pattern[?]](
        buf: RecordList[T, Q],
        pattern: Pattern[?],
    ): Chain[Either[RecordList[T, Q], RecordList[T, Q]]] =
      pattern match
        case pattern: Pattern.Tupled[?] =>
          pattern.elems.foldLeft(
            Chain.one(Right(buf): Either[RecordList[T, Q], RecordList[T, Q]]),
          ): (acc, elem) =>
            acc.flatMap:
              case Left(buf)  => Chain.one(Left(buf))
              case Right(buf) =>
                elem match
                  case pattern: Pattern[?] =>
                    scanPattern(buf, pattern)
                  case include: Pattern.Include[?] =>
                    scanPattern(buf, include.pattern)
                  case (_: Token, _) =>
                    // attrs have no impact on decision tree
                    Chain.one(Right(buf))
        case pattern: Pattern.alt[?] =>
          scanPattern(buf, pattern.left)
            ++ scanPattern(buf, pattern.right)
        case pattern: Pattern.map[?, ?] =>
          scanPattern(buf, pattern.pattern)
        case pattern: Pattern.tokenExact[?] =>
          Chain.one(Right(buf.ensureBranch.upsert(pattern.token)))
        case _: (Pattern.tokenAny[?] | Pattern.rep[?] | Pattern.rewrite[?]) =>
          Chain.one(Left(buf))
        case Pattern.endOfSpan =>
          Chain.one(Right(buf.ensureBranch.upsert(EndOfFieldsMarker)))
    end scanPattern

    private def scanQuery[T, Q[_] <: Query[?] | Pattern[?]](
        buf: RecordList[T, Q],
        query: Query[?],
    ): Chain[Either[RecordList[T, Q], RecordList[T, Q]]] =
      query match
        case Query.empty | (_: pure[?]) | Query.leftSibling |
            Query.rightSibling | Query.parent | (_: NodeAccessorQuery[?]) =>
          Chain.one(Right(buf))
        case query: on[?] =>
          scanPattern(buf, query.pattern)
        case query: map[?, ?] =>
          scanQuery(buf, query.query)
        case query: ap[?, ?] =>
          val left = scanQuery(buf, query.ff)
          val right = scanQuery(buf, query.fa)
          val leftMoved = left.exists(_.both ne buf)
          val rightMoved = right.exists(_.both ne buf)
          val hasLeft = left.exists(_.isLeft) || right.exists(_.isLeft)

          // If both sides "moved", then don't know what to do.
          // Exclude ourselves from decision tree at this point.
          if leftMoved && rightMoved
          then Chain.one(Left(buf))
          else if leftMoved
          then if hasLeft then left.map(_.forceLeft) else left
          else if hasLeft then right.map(_.forceLeft)
          else right
        case query: here[?] =>
          scanQuery(buf, query.place).map(_.forceLeft)
        case query: alt[?] =>
          scanQuery(buf, query.left)
            ++ scanQuery(buf, query.right)
        case Query.currentNode =>
          Chain.one(Left(buf))
        case _: Query.rewriteAll =>
          Chain.one(Left(buf))
    end scanQuery

    def fromQuery[T](query: Query[T]): FrozenRecordList[T, Query] =
      val acc = new RecordList[T, Query]
      query.altOptions.iterator.foreach(scanQuery(acc, _))
      acc.result().map(_.freeze)
    end fromQuery

    def fromPattern[T](pattern: Pattern[T]): FrozenRecordList[T, Pattern] =
      val acc = new RecordList[T, Pattern]
      pattern.altOptions.iterator.foreach(scanPattern(acc, _))
      acc.result().map(_.freeze)
    end fromPattern

    type RecordList[T, Q[_] <: Query[?] | Pattern[?]] =
      mutable.ListBuffer[Record[T, Q]]
    type FrozenRecordList[T, Q[_] <: Query[?] | Pattern[?]] =
      List[FrozenRecord[T, Q]]
    type Record[T, Q[_] <: Query[?] | Pattern[?]] = Q[T] | Branch[T, Q]
    final case class Branch[T, Q[_] <: Query[?] | Pattern[?]](
        map: mutable.HashMap[Token | EndOfFieldsMarker, RecordList[T, Q]],
    ):
      def upsert(key: Token | EndOfFieldsMarker): RecordList[T, Q] =
        map.getOrElseUpdate(key, mutable.ListBuffer.empty)
      end upsert
    end Branch

    extension [T, Q[_] <: Query[?] | Pattern[?]](recordList: RecordList[T, Q])
      def ensureBranch: Branch[T, Q] =
        recordList.lastOption
          .filter(_.isInstanceOf[Branch[T, Q]])
          .map(_.asInstanceOf[Branch[T, Q]])
          .getOrElse:
            val branch = new Branch[T, Q](
              mutable.HashMap.empty[Token | EndOfFieldsMarker, RecordList[T, Q]],
            )
            recordList += branch
            branch
      end ensureBranch
    end extension

    extension [T, Q[_] <: Query[?] | Pattern[?]](
        either: Either[RecordList[T, Q], RecordList[T, Q]]
    )
      def forceLeft: Either[RecordList[T, Q], RecordList[T, Q]] =
        if either.isRight
        then either.swap
        else either
      end forceLeft

      def both: RecordList[T, Q] =
        either.fold(identity, identity)
      end both
    end extension

    extension [T, Q[_] <: Query[?] | Pattern[?]](
        record: Record[T, Q]
    )(using TypeTest[Record[T, Q], Q[T]])
      def freeze: FrozenRecord[T, Q] =
        record match
          case Branch[T, Q](map) =>
            FrozenBranch(map.view.mapValues(_.map(_.freeze).result()).toMap)
          case q: Q[T] => q
      end freeze
    end extension

    type FrozenRecord[T, Q[_] <: Query[?] | Pattern[?]] = Q[T] |
      FrozenBranch[T, Q]
    final case class FrozenBranch[T, Q[_] <: Query[?] | Pattern[?]](
        map: Map[Token | EndOfFieldsMarker, FrozenRecordList[T, Q]],
    )

    extension [T, Q[_] <: Query[?] | Pattern[?]](
        recs: FrozenRecordList[T, Q]
    )(using TypeTest[FrozenRecord[T, Q], Q[T]])
      def run[U](
          nodeSpan: NodeSpan,
          runFn: Q[T] => Eval[Option[U]],
      ): Eval[Option[U]] =
        def impl(
            nodeSpan: NodeSpan,
            recs: FrozenRecordList[T, Q],
        ): Eval[Option[U]] =
          Foldable[List].collectFirstSomeM(recs):
            case query: Q[T] =>
              runFn(query)
            case FrozenBranch[T, Q](map) =>
              nodeSpan.expandRightOption(1) match
                case None =>
                  map.get(EndOfFieldsMarker) match
                    case None       => Eval.now(None)
                    case Some(recs) =>
                      impl(nodeSpan, recs)
                case Some(nodeSpan) =>
                  nodeSpan.last.tokenOption match
                    case None        => Eval.now(None)
                    case Some(token) =>
                      map.get(token) match
                        case None        => Eval.now(None)
                        case Some(query) =>
                          impl(nodeSpan, recs)
        end impl
        impl(nodeSpan, recs)
      end run
    end extension

    type EndOfFieldsMarker = EndOfFieldsMarker.type
    object EndOfFieldsMarker
  end DecisionTree

  object currentNode extends Query[Node]:
    protected def runQueryImpl(node: Node): Eval[Option[Node]] =
      Eval.now(Some(node))
    end runQueryImpl
  end currentNode

  transparent trait NodeAccessorQuery[T](query: Query[T]) extends Query[T]:
    protected def access(node: Node): Option[Node]
    protected final def runQueryImpl(node: Node): Eval[Option[T]] =
      access(node) match
        case None       => Eval.now(None)
        case Some(node) => Eval.defer(query.runQuery(node))
    end runQueryImpl
  end NodeAccessorQuery

  final class leftSibling[T](query: Query[T])
      extends NodeAccessorQuery[T](query):
    protected def access(node: Node): Option[Node] = node.leftSiblingOption
  end leftSibling

  object leftSibling extends Query[Node]:
    protected def runQueryImpl(node: Node): Eval[Option[Node]] =
      Eval.now(node.leftSiblingOption)
  end leftSibling

  final class rightSibling[T](query: Query[T])
      extends NodeAccessorQuery[T](query):
    protected def access(node: Node): Option[Node] = node.rightSiblingOption
  end rightSibling

  object rightSibling extends Query[Node]:
    protected def runQueryImpl(node: Node): Eval[Option[Node]] =
      Eval.now(node.rightSiblingOption)
  end rightSibling

  final class parent[T](query: Query[T]) extends NodeAccessorQuery[T](query):
    protected def access(node: Node): Option[Node] = node.parentOption
  end parent

  object parent extends Query[Node]:
    protected def runQueryImpl(node: Node): Eval[Option[Node]] =
      Eval.now(node.parentOption)
  end parent

  final class rewriteAll(rewrites: on[?]#rewrite*) extends Query[Node]:
    lazy val rewritesAgg =
      rewrites.view
        .map(_.pattern)
        .reduce(_ | _)
    end rewritesAgg

    protected def runQueryImpl(node: Node): Eval[Option[Node]] =
      def impl(
          nodeSpan: NodeSpan,
          madeChangesThisIteration: Boolean,
      ): Eval[NodeSpan] =
        Eval
          .defer(rewritesAgg.runPattern(nodeSpan))
          .flatMap:
            case None =>
              assert(nodeSpan.isEmpty)
              def firstChild = nodeSpan
                .expandRightOption(1)
                .map(_.head.children.asEmptyNodeSpan)
              end firstChild

              def firstAvailableParentsSiblingOrRoot(
                  nodeSpan: NodeSpan,
              ): NodeSpan =
                nodeSpan.parentOption match
                  case None         => nodeSpan
                  case Some(parent) =>
                    parent.rightSiblingOption match
                      case None =>
                        firstAvailableParentsSiblingOrRoot(
                          parent.emptyNodeSpanHere,
                        )
                      case Some(sibling) => sibling.emptyNodeSpanHere
              end firstAvailableParentsSiblingOrRoot

              val nextSpan = firstChild.getOrElse(
                firstAvailableParentsSiblingOrRoot(nodeSpan),
              )
              if nextSpan.parentOption.isEmpty
              then
                if madeChangesThisIteration
                then impl(nextSpan, madeChangesThisIteration = false)
                else Eval.now(nextSpan)
              else impl(nextSpan, madeChangesThisIteration)
            case Some(((), nodeSpan)) =>
              impl(nodeSpan.take(0), madeChangesThisIteration = true)
      end impl

      impl(node.asOrphan.emptyNodeSpanHere, madeChangesThisIteration = false)
        .map: nodeSpan =>
          val replacementNode =
            nodeSpan.headOption
              .getOrElse(nodeSpan.expandRightOption(1).get.head)
          // Put our parents back
          Some(node.replaceThis(replacementNode))
    end runQueryImpl
  end rewriteAll
end Query
