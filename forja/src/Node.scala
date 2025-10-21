package forja

import forja.util.{FastPatchTree, MonomorphicIndexedSeq}

import scala.annotation.publicInBinary
import scala.collection.concurrent
import scala.compiletime.asMatchable
import scala.language.experimental.into
import scala.quoted.{Expr, Quotes, Type, Varargs, quotes}
import scala.util.NotGiven

import Node.*

final class Node @publicInBinary private (
    private val impl: Node.NodeImpl,
    private val nodeParentInfo: NodeParentInfo,
):
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
        embed.extractOption(value)
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

  override def equals(that: Any): Boolean =
    that.asMatchable match
      case that: Node => this.impl == that.impl
      case _          => false
  end equals

  override def hashCode(): Int =
    this.impl.hashCode()
  end hashCode

  def query[T](query: Query[T]): Option[T] =
    query.runQuery(this).value
  end query
end Node

export Node.NodeSpan

object Node:
  def embed[T](value: T)(using embed: Embed[T]): Node =
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

  type NodeApplyArg = Node | (Token, Node)

  transparent inline def apply(inline token: Token, inline args: NodeApplyArg*)(
      using NotGiven[PatternContext],
  ): Node =
    ${ applyImplNode('token, 'args) }
  end apply

  private def applyImplNode(
      tokenExpr: Expr[Token],
      argsExpr: Expr[Seq[NodeApplyArg]],
  )(using Quotes): Expr[Node] =
    argsExpr match
      case Varargs(argExprs) =>
        val children = Seq.newBuilder[Expr[NodeImpl]]
        val attrs = Seq.newBuilder[Expr[(Token, NodeImpl)]]
        argExprs.foreach:
          case '{ $expr: Node } =>
            children += '{ $expr.impl }
          case '{ $expr: (Token, Node) } =>
            attrs += '{
              val pair = $expr
              (pair._1, pair._2.impl)
            }

        '{
          new Node(
            impl = NodeImpl.TokenNode(
              token = $tokenExpr,
              children = FastPatchTree(${ Varargs(children.result()) }*),
              attrs = Map(${ Varargs(attrs.result()) }*),
            ),
            nodeParentInfo = NodeParentInfo.Orphan,
          )
        }
  end applyImplNode

  type PatternApplyArg = Pattern[?] | Pattern.Include[?] |
    (Token, Pattern[?] | Pattern.Include[?])

  transparent inline def apply(
      inline token: Token,
      inline args: PatternApplyArg*,
  )(using inline ctx: PatternContext): Pattern[Tuple] =
    Pattern.tokenExact(token, NodeSpan(args*))
  end apply

  transparent inline def apply(inline args: PatternApplyArg*)(using
      inline ctx: PatternContext,
  ): Pattern[Tuple] =
    Pattern.tokenAny(NodeSpan(args*))
  end apply

  trait Embed[T]:
    def extractOption(value: Any): Option[T]
  end Embed

  object Embed:
    private[Node] val embedByClass = concurrent.TrieMap[Class[?], Embed[?]]()
  end Embed

  sealed trait Attrs extends Map[Token, Node]:

  end Attrs

  private enum NodeParentInfo:
    case IndexParent(parent: Node, index: Int)
    case AttrParent(parent: Node, attr: Token)
    case Orphan
  end NodeParentInfo

  private enum NodeImpl:
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
    case EmbedNode(value: Any)
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
    def parentOption: Option[Node]
  end NodeSpan

  object NodeSpan:
    transparent inline def apply(inline args: PatternApplyArg*)(using
        PatternContext,
    ): Pattern[Tuple] =
      ${ applyImplPattern('args) }
    end apply

    private def applyImplPattern(argsExpr: Expr[Seq[PatternApplyArg]])(using
        Quotes,
    ): Expr[Pattern[Tuple]] =
      import quotes.reflect.*
      argsExpr match
        case Varargs(argExprs) =>
          // make a pattern
          val includes = argExprs.collect:
            case '{ $_ : Pattern.Include[t] }          => '{ ??? : t }
            case '{ $_ : (Token, Pattern.Include[t]) } => '{ ??? : t }
          end includes

          Expr.ofTupleFromSeq(includes) match
            case '{ $_ : includesTuple } =>
              '{
                Pattern
                  .Tupled($argsExpr*)
                  .asInstanceOf[Pattern[Tuple & includesTuple]]
              }
    end applyImplPattern
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
      val slicedIndices = indices.slice(from, until)
      IndexedParentNodeSpan(parent, slicedIndices.min, slicedIndices.max)
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
