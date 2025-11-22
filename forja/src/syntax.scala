package forja

import forja.util.{FastPatchTree, TupleOf}

import scala.collection.mutable
import scala.util.NotGiven

object syntax:
  export Wf.TokenWf
  export Query.on

  object unchanged
  case object `...`
  type `...` = `...`.type

  transparent inline def cc(using c: Context): c.type = c

  sealed abstract class Context:
    given localContext: this.type = this

    trait NodeApply[-T, +U] extends (T => U)
    trait NodeSpanApply[-T, +U] extends (T => U)
  end Context
  object Context:
    given default: ValueContext.type = ValueContext
  end Context

  object ValueContext extends Context:
    def lit[T <: Matchable: Node.Embed](value: T): Node =
      Node.embed(value)
    end lit

    sealed trait NodeApplyImpl[-T]:
      def children(arg: T, builder: mutable.Growable[Node.NodeImpl]): Unit
      def attrs(arg: T, builder: mutable.Growable[(Token, Node.NodeImpl)]): Unit
    end NodeApplyImpl

    sealed trait NodeSpanApplyImpl[-T] extends NodeApplyImpl[T]:
      final def attrs(
          arg: T,
          builder: mutable.Growable[(Token, Node.NodeImpl)],
      ): Unit = ()
    end NodeSpanApplyImpl

    given nodeSpanApplyImplNode: NodeSpanApplyImpl[Node]:
      def children(arg: Node, builder: mutable.Growable[Node.NodeImpl]): Unit =
        builder += arg.impl
      end children
    end nodeSpanApplyImplNode

    given nodeSpanApplyImplNodeIter
        : [T <: IterableOnce[Node]] => NodeSpanApplyImpl[IterableOnce[Node]]:
      def children(
          arg: IterableOnce[Node],
          builder: mutable.Growable[Node.NodeImpl],
      ): Unit =
        builder ++= arg.iterator.map(_.impl)
      end children
    end nodeSpanApplyImplNodeIter

    given nodeApplyImplAttr: NodeApplyImpl[(Token, Node)]:
      def children(
          arg: (Token, Node),
          builder: mutable.Growable[Node.NodeImpl],
      ): Unit = ()
      def attrs(
          arg: (Token, Node),
          builder: mutable.Growable[(Token, Node.NodeImpl)],
      ): Unit =
        builder += ((arg._1, arg._2.impl))
      end attrs
    end nodeApplyImplAttr

    given nodeSpanApplyImplEmbed
        : [T <: Matchable: Node.Embed] => NodeSpanApplyImpl[T]:
      def children(arg: T, builder: mutable.Growable[Node.NodeImpl]): Unit =
        builder += Node.embed(arg).impl
      end children
    end nodeSpanApplyImplEmbed

    given nodeApply
        : [Rest <: Tuple] => (impls: TupleOf[Tuple.Map[Rest, NodeApplyImpl]])
            => NodeApply[Token *: Rest, Node]:
      def apply(arg: Token *: Rest): Node =
        val childrenBuilder = FastPatchTree.newBuilder[Node.NodeImpl]
        val attrsBuilder = Map.newBuilder[Token, Node.NodeImpl]
        (arg.productIterator
          .drop(1)
          .zip(impls.value.productIterator))
          .foreach: (arg, impl) =>
            impl.asInstanceOf[NodeApplyImpl[Any]].children(arg, childrenBuilder)
            impl.asInstanceOf[NodeApplyImpl[Any]].attrs(arg, attrsBuilder)
        new Node(
          impl = Node.NodeImpl.TokenNode(
            token = arg.head,
            children = childrenBuilder.result(),
            attrs = attrsBuilder.result(),
          ),
          nodeParentInfo = Node.NodeParentInfo.Orphan,
        )
      end apply
    end nodeApply

    given nodeSpanApply
        : [T <: Tuple] => (impls: TupleOf[Tuple.Map[T, NodeSpanApplyImpl]])
            => NodeSpanApply[T, Seq[Node]]:
      def apply(arg: T): Seq[Node] =
        val builder = Seq.newBuilder[Node.NodeImpl]
        (arg.productIterator
          .zip(impls.value.productIterator))
          .foreach: (arg, impl) =>
            impl.asInstanceOf[NodeSpanApplyImpl[Any]].children(arg, builder)

        builder
          .result()
          .map: impl =>
            new Node(
              impl = impl,
              nodeParentInfo = Node.NodeParentInfo.Orphan,
            )
      end apply
    end nodeSpanApply
  end ValueContext

  object PatternContext extends Context:
    def lit[T <: Matchable: Node.Embed](value: T, values: T*): Pattern[T] =
      values.foldLeft(new Pattern.EmbedLiteralPattern(value): Pattern[T]):
        (acc, v) => acc | new Pattern.EmbedLiteralPattern(v)
    end lit

    def embed[T <: Matchable: Node.Embed]: Pattern[T] =
      Pattern.embed[T]
    end embed

    def rep[T](elem: Pattern[T]): Pattern[List[T]] =
      Pattern.rep(elem)
    end rep

    def rep1[T](elem: Pattern[T]): Pattern[List[T]] =
      NodeSpan(+elem, +cc.rep(elem)).map(_ :: _)
    end rep1

    def not[T](elem: Pattern[T]): Pattern[Unit] =
      Pattern.not(elem)
    end not

    sealed trait NodeSpanApplyImpl[-T <: Tuple, +U <: Tuple]:
      def tupleArity: Int
      def cases(arg: T): List[Pattern.Tupled.Case]
      final def apply(total: Boolean, arg: T): Pattern[U] =
        Pattern.Tupled(
          isTotal = total,
          tupleArity = tupleArity,
          cases = cases(arg),
        )
      end apply
    end NodeSpanApplyImpl

    given nodeSpanApplyEmpty: NodeSpanApplyImpl[EmptyTuple, EmptyTuple]:
      def tupleArity: Int = 0
      def cases(arg: EmptyTuple) = Nil
    end nodeSpanApplyEmpty

    given nodeSpanApplyIncludeNonTuple: [T, Rest <: Tuple, RestU <: Tuple]
      => NotGiven[T <:< Tuple] => (impl: NodeSpanApplyImpl[Rest, RestU])
        => NodeSpanApplyImpl[Pattern.Include[T] *: Rest, T *: RestU]:
      def tupleArity: Int = 1 + impl.tupleArity
      def cases(arg: Pattern.Include[T] *: Rest) =
        Pattern.Tupled.IncludeCase(arg.head.pattern)
          :: impl.cases(arg.tail)
    end nodeSpanApplyIncludeNonTuple

    given nodeSpanApplyIncludeTuple: [T <: Tuple, Rest <: Tuple, RestU <: Tuple]
      => (includedSize: ValueOf[Tuple.Size[T]])
      => (
          impl: NodeSpanApplyImpl[Rest, RestU],
    ) => NodeSpanApplyImpl[Pattern.Include[T] *: Rest, Tuple.Concat[T, RestU]]:
      def tupleArity: Int = includedSize.value + impl.tupleArity
      def cases(arg: Pattern.Include[T] *: Rest) =
        Pattern.Tupled.IncludeTupleCase(arg.head.pattern)
          :: impl.cases(arg.tail)
    end nodeSpanApplyIncludeTuple

    given nodeSpanApplySkip: [T, Rest <: Tuple, RestU <: Tuple]
      => (
          impl: NodeSpanApplyImpl[Rest, RestU],
    ) => NodeSpanApplyImpl[Pattern[T] *: Rest, RestU]:
      def tupleArity: Int = impl.tupleArity
      def cases(arg: Pattern[T] *: Rest) =
        Pattern.Tupled.SkipCase(arg.head)
          :: impl.cases(arg.tail)
    end nodeSpanApplySkip

    given `nodeSpanApply...`: [T, Rest <: Tuple, RestU <: Tuple]
      => (
          impl: NodeSpanApplyImpl[Rest, RestU],
    ) => NodeSpanApplyImpl[`...`.type *: Rest, RestU]:
      def tupleArity: Int = impl.tupleArity
      def cases(arg: `...`.type *: Rest) =
        Pattern.Tupled.WildcardCase
          :: impl.cases(arg.tail)
    end `nodeSpanApply...`

    given nodeSpanApply: [T <: Tuple, U <: Tuple]
      => (impl: NodeSpanApplyImpl[T, U]) => NodeSpanApply[T, Pattern[U]]:
      def apply(arg: T): Pattern[U] =
        impl(total = false, arg = arg)
      end apply
    end nodeSpanApply

    given nodeApplyToken
        : [Rest <: Tuple, U <: Tuple] => (impl: NodeSpanApplyImpl[Rest, U])
            => NodeApply[Token *: Rest, Pattern[U]]:
      def apply(arg: Token *: Rest): Pattern[U] =
        Pattern.tokenExact(arg.head, impl(total = true, arg = arg.tail))
      end apply
    end nodeApplyToken

    given nodeApplyAny
        : [T <: Tuple, U <: Tuple] => NotGiven[Tuple.Head[T] <:< Token]
          => (impl: NodeSpanApplyImpl[T, U]) => NodeApply[T, Pattern[U]]:
      def apply(arg: T): Pattern[U] =
        Pattern.tokenAny(impl(total = true, arg = arg))
      end apply
    end nodeApplyAny
  end PatternContext

  object WfContext extends Context:
    def embed[T <: Matchable: Node.Embed]: Wf.EmbedWf[T] =
      Wf.EmbedWf[T]

    def rep[T](elem: Wf.Shape): Wf.RepeatedShape =
      new Wf.RepeatedShape(elem)
  end WfContext
end syntax
