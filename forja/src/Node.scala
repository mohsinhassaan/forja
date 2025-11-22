package forja

import java.io.{ByteArrayOutputStream, OutputStream}
import java.nio.charset.StandardCharsets

import forja.util.{FastPatchTree, MonomorphicIndexedSeq}

import scala.annotation.publicInBinary
import scala.collection.concurrent
import scala.compiletime.asMatchable
import scala.reflect.TypeTest

import Node.*

final class Node @publicInBinary private[forja] (
    private[forja] val impl: Node.NodeImpl,
    private val nodeParentInfo: NodeParentInfo,
) extends geny.Writable:
  /* Regular nodeParentInfo may not refer to a parent that refers back to impl.
   * If we want a parent ref that is up to date, this will lazily populate such
   * a thing.
   * Important benefit of doing it like this: this lazy val is internal and does
   * not invoke any other lazy vals, so it will always expand just 1 parent
   * node. Making nodeParentInfo itself lazy could lead to arbitrary recursion. */
  private lazy val nodeParentInfoStable =
    nodeParentInfo match
      case NodeParentInfo.IndexParent(parent, index) =>
        val parentImpl = parent.impl.asInstanceOf[NodeImpl.TokenNode]
        if parentImpl.children(index) eq this.impl
        then nodeParentInfo
        else
          NodeParentInfo.IndexParent(
            new Node(
              impl = parentImpl.copy(children =
                parentImpl.children.updated(index, this.impl),
              ),
              nodeParentInfo = parent.nodeParentInfo,
            ),
            index,
          )
      case NodeParentInfo.AttrParent(parent, attr) =>
        val parentImpl = parent.impl.asInstanceOf[NodeImpl.TokenNode]
        if parentImpl.attrs(attr) eq this.impl
        then nodeParentInfo
        else
          NodeParentInfo.AttrParent(
            new Node(
              impl = parentImpl.copy(attrs =
                parentImpl.attrs.updated(attr, this.impl),
              ),
              nodeParentInfo = parent.nodeParentInfo,
            ),
            attr,
          )
      case NodeParentInfo.Orphan =>
        NodeParentInfo.Orphan
  end nodeParentInfoStable

  def tokenOption: Option[Token] =
    impl match
      case NodeImpl.TokenNode(token, children, attrs)   => Some(token)
      case _: (NodeImpl.EmbedNode | NodeImpl.ErrorNode) => None
  end tokenOption

  def valueOption[T](using embed: Embed[T]): Option[T] =
    impl match
      case _: (NodeImpl.TokenNode | NodeImpl.ErrorNode) => None
      case NodeImpl.EmbedNode(value)                    =>
        import embed.typeTest
        value match
          case value: T => Some(value)
          case _        => None
        end match
    end match
  end valueOption

  def valueOptionRaw: Option[Any] =
    impl match
      case _: (NodeImpl.TokenNode | NodeImpl.ErrorNode) => None
      case NodeImpl.EmbedNode(value)                    =>
        Some(value)
  end valueOptionRaw

  def isError: Boolean =
    impl match
      case _: NodeImpl.ErrorNode                        => true
      case _: (NodeImpl.EmbedNode | NodeImpl.TokenNode) => false
  end isError

  export impl.containsError

  def errorMsgOption: Option[String] =
    impl match
      case NodeImpl.ErrorNode(msg, _*)                  => Some(msg)
      case _: (NodeImpl.EmbedNode | NodeImpl.TokenNode) => None
  end errorMsgOption

  def errorNodesOption: Option[Seq[Node]] =
    impl match
      case NodeImpl.ErrorNode(msg, nodes*)              => Some(nodes)
      case _: (NodeImpl.EmbedNode | NodeImpl.TokenNode) => None
  end errorNodesOption

  def parentOption: Option[Node] =
    nodeParentInfoStable match
      case NodeParentInfo.IndexParent(parent, index) => Some(parent)
      case NodeParentInfo.AttrParent(parent, attr)   => Some(parent)
      case NodeParentInfo.Orphan                     => None
  end parentOption

  def leftSiblingOption: Option[Node] =
    nodeParentInfoStable match
      case NodeParentInfo.IndexParent(parent, index) =>
        parent.children.lift(index - 1)
      case _: (NodeParentInfo.AttrParent | NodeParentInfo.Orphan.type) =>
        None
  end leftSiblingOption

  def rightSiblingOption: Option[Node] =
    nodeParentInfoStable match
      case NodeParentInfo.IndexParent(parent, index) =>
        parent.children.lift(index + 1)
      case _: (NodeParentInfo.AttrParent | NodeParentInfo.Orphan.type) =>
        None
  end rightSiblingOption

  def asOrphan: Node =
    new Node(
      impl = impl,
      nodeParentInfo = NodeParentInfo.Orphan,
    )

  def replaceThis(replacement: Node): Node =
    new Node(
      impl = replacement.impl,
      nodeParentInfo = nodeParentInfo,
    )
  end replaceThis

  def children: NodeChildren = NodeChildren(this)
  def attrs: NodeAttrs = NodeAttrs(this)

  def emptyNodeSpanHere: NodeSpan = nodeParentInfoStable match
    case NodeParentInfo.IndexParent(parent, index) =>
      IndexedParentNodeSpan(parent, index, 0)
    case _: (NodeParentInfo.AttrParent | NodeParentInfo.Orphan.type) =>
      SingletonNodeSpan(this, false)
  end emptyNodeSpanHere

  def singletonNodeSpanHere: NodeSpan = nodeParentInfoStable match
    case NodeParentInfo.IndexParent(parent, index) =>
      IndexedParentNodeSpan(parent, index, 1)
    case _: (NodeParentInfo.AttrParent | NodeParentInfo.Orphan.type) =>
      SingletonNodeSpan(this, true)
  end singletonNodeSpanHere

  def query[T](query: Query[T]): Option[T] =
    query.runQuery(this).value
  end query

  override def equals(that: Any): Boolean =
    that.asMatchable match
      case that: Node => this.impl == that.impl
      case _          => false
  end equals

  override def hashCode(): Int =
    this.impl.hashCode()
  end hashCode

  override def toString(): String =
    val out = ByteArrayOutputStream()
    writeBytesTo(out)
    out.toString(StandardCharsets.UTF_8)
    // impl match
    //   case NodeImpl.TokenNode(token, _, _) =>
    //     s"$token(${(this.children.view.map(_.toString()) ++ this.attrs.view.map(
    //         (k, v) => s"$k -> $v",
    //       )).mkString(", ")})"
    //   case NodeImpl.EmbedNode(value) =>
    //     val embed = Node.Embed.embedByValue(value)
    //     s"${value.getClass().getName()}($value)"
    //   case NodeImpl.ErrorNode(msg, nodes*) =>
    //     s"#error(\"$msg\", ${nodes.mkString(", ")})"
  end toString

  def writeBytesTo(out_ : OutputStream): Unit =

    object out extends OutputStream:
      private var indent = 0
      def write(b: Int): Unit =
        b match
          case '\n' =>
            out_.write('\n')
            (0 until indent).foreach(_ => out_.write(' '))
          case b =>
            out_.write(b)
      end write

      def indentedBy[T](amt: Int = 2)(fn: => T): T =
        try
          indent += amt
          fn
        finally indent -= amt
      end indentedBy
    end out

    var isFirstLine = true
    def nl(): Unit =
      if isFirstLine
      then isFirstLine = false
      else out.write('\n')
    end nl

    def writeImpl(impl: NodeImpl): Unit =
      nl()
      impl match
        case NodeImpl.TokenNode(token, children, attrs) =>
          out.write('>')
          out.write(token.fullName.getBytes(StandardCharsets.UTF_8))
          out.indentedBy():
            children.foreach: impl =>
              writeImpl(impl)
            attrs.keys.toArray
              .sortBy(_.fullName)
              .foreach: k =>
                nl()
                out.write('?')
                out.write(k.fullName.getBytes(StandardCharsets.UTF_8))
                out.indentedBy():
                  writeImpl(attrs(k))
        case NodeImpl.EmbedNode(value) =>
          val embed = Embed.embedByValue(value)

          out.write('~')
          out.write(embed.getClass().getName().getBytes(StandardCharsets.UTF_8))
          out.indentedBy():
            nl()
            embed.writeBytesTo(value, out)
        case NodeImpl.ErrorNode(msg, nodes*) =>
          out.indentedBy():
            msg.linesWithSeparators
              .foreach: line =>
                out.write('!')
                out.write(line.getBytes(StandardCharsets.UTF_8))
            nodes.foreach: impl =>
              writeImpl(impl.impl)
      end match
    end writeImpl

    writeImpl(impl)
  end writeBytesTo
end Node

export Node.NodeSpan

object Node:
  def embed[T <: Matchable](value: T)(using embed: Embed[T]): Node =
    Embed.embedByClass.putIfAbsent(value.getClass(), embed)
    new Node(
      impl = NodeImpl.EmbedNode(value),
      nodeParentInfo = NodeParentInfo.Orphan,
    )
  end embed

  def error(msg: String)(nodes: Node*): Node =
    new Node(
      impl = NodeImpl.ErrorNode(msg, nodes*),
      nodeParentInfo = NodeParentInfo.Orphan,
    )
  end error

  inline def applyTupled[Tp <: Tuple, U](tp: Tp)(using
      ctx: syntax.Context,
  )(using app: ctx.NodeApply[Tp, U]): U = app(tp)

  // format: off
  inline def apply[U]()(using ctx: syntax.Context)(using app: ctx.NodeApply[EmptyTuple, U]): U = app(EmptyTuple)
  inline def apply[T1, U](t1: T1)(using ctx: syntax.Context)(using app: ctx.NodeApply[Tuple1[T1], U]): U = app(Tuple1(t1))
  // %%replicate22
  inline def apply[T1, T2, U](t1: T1, t2: T2)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2), U]): U = app((t1, t2))
  inline def apply[T1, T2, T3, U](t1: T1, t2: T2, t3: T3)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3), U]): U = app((t1, t2, t3))
  inline def apply[T1, T2, T3, T4, U](t1: T1, t2: T2, t3: T3, t4: T4)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4), U]): U = app((t1, t2, t3, t4))
  inline def apply[T1, T2, T3, T4, T5, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5), U]): U = app((t1, t2, t3, t4, t5))
  inline def apply[T1, T2, T3, T4, T5, T6, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6), U]): U = app((t1, t2, t3, t4, t5, t6))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7), U]): U = app((t1, t2, t3, t4, t5, t6, t7))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18, t19: T19)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18, t19: T19, t20: T20)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18, t19: T19, t20: T20, t21: T21)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21))
  inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18, t19: T19, t20: T20, t21: T21, t22: T22)(using ctx: syntax.Context)(using app: ctx.NodeApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22))
  // format: on

  trait Embed[T](using val typeTest: TypeTest[Matchable, T]):
    self: Singleton =>
    locally:
      val modField = getClass().getField("MODULE$")
      require(
        modField ne null,
        s"${getClass().getName()} must have a static MODULE$$ field, or it will not be deserializable",
      )
      require(
        modField.get(null) eq self,
        s"${getClass().getName()}'s static MODULE$$ field must refer to itself",
      )
    // Compiler crash?
    // given typeTest: TypeTest[Matchable, T] = deferred
    def prettyString(value: T): String
    def writeBytesTo(value: T, out: OutputStream): Unit
  end Embed

  object Embed:
    private[Node] val embedByClass = concurrent.TrieMap[Class[?], Embed[?]]()
    private[forja] def embedByValue[T](value: T): Node.Embed[T] =
      embedByClass(value.getClass()).asInstanceOf[Node.Embed[T]]
    end embedByValue

    trait EmbedPrimitive[T] extends Embed[T]:
      self: Singleton =>
      def prettyString(value: T): String =
        s"${value.getClass()}($value)"
      end prettyString
      def writeBytesTo(value: T, out: OutputStream): Unit =
        out.write(value.toString().getBytes(StandardCharsets.UTF_8))
      end writeBytesTo
    end EmbedPrimitive

    given embedBoolean: EmbedPrimitive[Boolean] {}
    given embedByte: EmbedPrimitive[Byte] {}
    given embedInt: EmbedPrimitive[Int] {}
    given embedLong: EmbedPrimitive[Long] {}
    given embedFloat: EmbedPrimitive[Float] {}
    given embedDouble: EmbedPrimitive[Double] {}
    given embedChar: EmbedPrimitive[Char] {}
  end Embed

  sealed trait Attrs extends Map[Token, Node]:

  end Attrs

  private[forja] enum NodeParentInfo:
    case IndexParent(parent: Node, index: Int)
    case AttrParent(parent: Node, attr: Token)
    case Orphan
  end NodeParentInfo

  private[forja] enum NodeImpl:
    this match
      case _: (TokenNode | ErrorNode) =>
      // nothing to do here
      case EmbedNode(value) =>
        assert(
          Embed.embedByClass.contains(value.getClass()),
          s"!!internal error: using unregistered embed of ${value.getClass()}",
        )

    case TokenNode(
        token: Token,
        children: FastPatchTree[NodeImpl],
        attrs: Map[Token, NodeImpl],
    )
    case EmbedNode(value: Matchable)
    case ErrorNode(msg: String, nodes: Node*)

    val containsError: Boolean =
      this match
        case _: ErrorNode                      => true
        case _: EmbedNode                      => false
        case TokenNode(token, children, attrs) =>
          children.exists(_.containsError)
          || attrs.values.exists(_.containsError)
    end containsError
  end NodeImpl

  final class NodeChildren private[Node] (parent: Node)
      extends IndexedSeq[Node]:
    private[Node] val implSeq: FastPatchTree[NodeImpl] = parent.impl match
      case NodeImpl.TokenNode(token, children, attrs)   => children
      case _: (NodeImpl.EmbedNode | NodeImpl.ErrorNode) => FastPatchTree.empty
    end implSeq

    def apply(i: Int): Node =
      new Node(
        impl = implSeq(i),
        nodeParentInfo = NodeParentInfo.IndexParent(parent, i),
      )
    end apply

    export implSeq.length

    def asEmptyNodeSpan: NodeSpan =
      IndexedParentNodeSpan(parent, 0, 0)
    end asEmptyNodeSpan

    def asNodeSpan: NodeSpan =
      IndexedParentNodeSpan(parent, 0, length)
    end asNodeSpan
  end NodeChildren

  final class NodeAttrs private[Node] (parent: Node) extends Map[Token, Node]:
    private[Node] val implMap: Map[Token, NodeImpl] = parent.impl match
      case NodeImpl.TokenNode(token, children, attrs)   => attrs
      case _: (NodeImpl.EmbedNode | NodeImpl.ErrorNode) => Map.empty
    end implMap

    def get(key: Token): Option[Node] =
      implMap
        .get(key)
        .map: impl =>
          new Node(
            impl = impl,
            nodeParentInfo = NodeParentInfo.AttrParent(parent, key),
          )
    end get

    def iterator: Iterator[(Token, Node)] =
      implMap.iterator
        .map: (token, impl) =>
          token -> new Node(
            impl = impl,
            nodeParentInfo = NodeParentInfo.AttrParent(parent, token),
          )
    end iterator

    private lazy val reifiedMap = iterator.toMap
    export reifiedMap.{updated, removed}
  end NodeAttrs

  sealed trait NodeSpan extends MonomorphicIndexedSeq[Node, NodeSpan]:
    def replaceThis(nodes: Iterable[Node]): NodeSpan
    def expandLeftOption(n: Int): Option[NodeSpan]
    def expandRightOption(n: Int): Option[NodeSpan]
    def expandRightMax: NodeSpan
    def parentOption: Option[Node]
    override protected def className: String = "NodeSpan"
  end NodeSpan

  object NodeSpan:
    inline def applyTupled[Tp <: Tuple, U](tp: Tp)(using
        ctx: syntax.Context,
    )(using app: ctx.NodeSpanApply[Tp, U]): U = app(tp)

    // format: off
    inline def apply[U]()(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[EmptyTuple, U]): U = app(EmptyTuple)
    inline def apply[T1, U](t1: T1)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[Tuple1[T1], U]): U = app(Tuple1(t1))
    // %%replicate22
    inline def apply[T1, T2, U](t1: T1, t2: T2)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2), U]): U = app((t1, t2))
    inline def apply[T1, T2, T3, U](t1: T1, t2: T2, t3: T3)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3), U]): U = app((t1, t2, t3))
    inline def apply[T1, T2, T3, T4, U](t1: T1, t2: T2, t3: T3, t4: T4)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4), U]): U = app((t1, t2, t3, t4))
    inline def apply[T1, T2, T3, T4, T5, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5), U]): U = app((t1, t2, t3, t4, t5))
    inline def apply[T1, T2, T3, T4, T5, T6, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6), U]): U = app((t1, t2, t3, t4, t5, t6))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7), U]): U = app((t1, t2, t3, t4, t5, t6, t7))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18, t19: T19)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18, t19: T19, t20: T20)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18, t19: T19, t20: T20, t21: T21)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21))
    inline def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, U](t1: T1, t2: T2, t3: T3, t4: T4, t5: T5, t6: T6, t7: T7, t8: T8, t9: T9, t10: T10, t11: T11, t12: T12, t13: T13, t14: T14, t15: T15, t16: T16, t17: T17, t18: T18, t19: T19, t20: T20, t21: T21, t22: T22)(using ctx: syntax.Context)(using app: ctx.NodeSpanApply[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22), U]): U = app((t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22))
    // format: on
  end NodeSpan

  private final class SingletonNodeSpan(node: Node, includesMe: Boolean)
      extends NodeSpan:
    def apply(i: Int): Node =
      if includesMe && i == 0
      then node
      else throw IndexOutOfBoundsException(s"$i out of bounds")
    end apply

    def length: Int = if includesMe then 1 else 0

    protected def sliceImpl(from: Int, until: Int): NodeSpan =
      val slicedIndices = indices.slice(from, until)
      if includesMe && slicedIndices.isEmpty
      then SingletonNodeSpan(node, false)
      else this
    end sliceImpl

    def expandLeftOption(n: Int): Option[NodeSpan] =
      if n != 0
      then None
      else Some(this)
    end expandLeftOption

    def expandRightOption(n: Int): Option[NodeSpan] =
      if !includesMe && n == 1
      then Some(SingletonNodeSpan(node, true))
      else if n == 0
      then Some(this)
      else None
    end expandRightOption

    def expandRightMax: NodeSpan =
      if !includesMe
      then SingletonNodeSpan(node, true)
      else this
    end expandRightMax

    def replaceThis(nodes: Iterable[Node]): NodeSpan =
      require(includesMe, "tried to rewrite an empty orphaned span")
      require(
        nodes.size == 1,
        s"tried to make an orphaned span have a number of nodes other than 1 (${nodes.size})",
      )
      SingletonNodeSpan(nodes.head, true)
    end replaceThis

    def parentOption: Option[Node] = None
  end SingletonNodeSpan

  private final class IndexedParentNodeSpan(
      parent: Node,
      start: Int,
      val length: Int,
  ) extends NodeSpan:
    def apply(i: Int): Node =
      parent.children.view
        .slice(start, start + length)
        .apply(i)
    end apply

    protected def sliceImpl(from: Int, until: Int): NodeSpan =
      val indicesInParent = parent.children.indices.slice(start, start + length)
      val slicedIndices = indicesInParent.slice(from, until)
      IndexedParentNodeSpan(
        parent,
        slicedIndices.headOption.getOrElse(
          (start + from).max(start + slicedIndices.length),
        ),
        slicedIndices.length,
      )
    end sliceImpl

    def expandLeftOption(n: Int): Option[NodeSpan] =
      if start - n >= 0
      then Some(IndexedParentNodeSpan(parent, start - n, length + n))
      else None
    end expandLeftOption

    def expandRightOption(n: Int): Option[NodeSpan] =
      if start + length + n <= parent.children.length
      then Some(IndexedParentNodeSpan(parent, start, length + n))
      else None
    end expandRightOption

    def expandRightMax: NodeSpan =
      val fullView = parent.children.view.drop(start)
      if fullView.length != length
      then IndexedParentNodeSpan(parent, start, fullView.length)
      else this
    end expandRightMax

    def replaceThis(nodes: Iterable[Node]): NodeSpan =
      val token = parent.tokenOption.get

      IndexedParentNodeSpan(
        parent = new Node(
          impl = NodeImpl.TokenNode(
            token = token,
            children = parent.children.implSeq
              .patch(start, nodes.iterator.map(_.impl), length),
            attrs = parent.attrs.implMap,
          ),
          nodeParentInfo = parent.nodeParentInfo,
        ),
        start = start,
        length = nodes.size,
      )
    end replaceThis

    def parentOption: Option[Node] = Some(parent)
  end IndexedParentNodeSpan
end Node
