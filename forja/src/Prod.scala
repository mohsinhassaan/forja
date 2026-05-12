package forja

import scala.deriving.Mirror
import scala.compiletime.ops.int.`+`
import scala.util.NotGiven
import scala.reflect.ClassTag
import scala.reflect.TypeTest
import scala.reflect.Typeable
import scala.compiletime.deferred
import scala.quoted.Type
import scala.quoted.Quotes
import scala.quoted.Expr
import forja.util.TupleOf
import java.util.concurrent.atomic.AtomicInteger
import scala.compiletime.summonFrom
import scala.compiletime.summonInline
import scala.compiletime.erasedValue
import scala.compiletime.asMatchable
import izumi.reflect.Tag
import scala.annotation.publicInBinary

into sealed abstract class Prod[T]:
  private val epoch = Prod.epoch.get()

  override def equals(obj: Any): Boolean = ???

  def tag: Tag[T]

  def reify(): T

  final def rewritePre(rw: Prod.Rewrite): Prod[T] =
    rw.rewrite(this).rewriteChildren([u] => uu => uu.rewritePre(rw))
  end rewritePre

  final def rewritePost(rw: Prod.Rewrite): Prod[T] =
    rw.rewrite(rewriteChildren([u] => uu => uu.rewritePost(rw)))
  end rewritePost

  final def fixpoint(rw: Prod.Rewrite): Prod[T] =
    def impl[T](self: Prod[T], rw: Prod.Rewrite, fromEpoch: Int): Prod[T] =
      if self.epoch >= fromEpoch
      then
        val nextUnstableEpoch = Prod.epoch.incrementAndGet()
        var currSelf = self
        while
          val prevSelf = currSelf
          currSelf = rw.rewrite(currSelf)
          prevSelf ne currSelf
        do ()
        currSelf = currSelf.rewriteChildren([u] => uu => impl(uu, rw, fromEpoch))
        if currSelf ne self
        then impl(currSelf, rw, fromEpoch = nextUnstableEpoch)
        else self
      else self
    end impl

    // Start epoch at -1 for unconditional scan.
    // Rescans from there will only account for nodes created on or after the original epoch.
    // That is, we will only look at nodes that have any possibility of coming from a rewrite
    // we just performed.
    impl(this, rw, fromEpoch = -1)
  end fixpoint

  protected def rewriteChildren(fn: [U] => Prod[U] => Prod[U]): Prod[T]

  protected def unapplyLifted[U](ops: Prod.LiftableOps[U]): Option[ops.U]

  def cast[U : Tag]: Prod[U]

  final def upcast[U >: T : Tag]: Prod[U] = cast[U]
end Prod

object Prod:
  private val epoch = AtomicInteger(0)

  given lift: [T] => (liftable: Liftable[T]) => Conversion[T, Prod[T]]:
    def apply(t: T): Prod[T] = liftable.lift(t)
  end lift

  def apply[T](using ops: LiftableOps.Aux[T, EmptyTuple])(): Prod[T] =
    Lifted(ops, EmptyTuple)
  end apply

  given upcast: [T : Tag, U <: T] => Conversion[Prod[U], Prod[T]]:
    def apply(prod: Prod[U]): Prod[T] = prod.upcast[T]
  end upcast

  type StripTuple1[U] = U match
    case Tuple1[u] => u
    case _ => U
  end StripTuple1

  inline def apply[T](using ops: LiftableOps[T])(u: StripTuple1[ops.U]): Prod[T] =
    inline erasedValue[ops.U & Matchable] match
      case _: Tuple1[_] => applyImpl(Tuple1(u).asInstanceOf)
      case _ => applyImpl(u.asInstanceOf)
    end match
  end apply

  private def applyImpl[T](using ops: LiftableOps[T])(u: ops.U): Prod[T] =
    Lifted(ops, u)
  end applyImpl

  inline def unapply[T](prod: Prod[?])(using ops: LiftableOps[T]): Option[StripTuple1[ops.U]] =
    prod.unapplyLifted(ops).map: x =>
      x.asMatchable match
        case Tuple1(u) => u.asInstanceOf[StripTuple1[ops.U]]
        case u => u.asInstanceOf[StripTuple1[ops.U]]
      end match
  end unapply

  final case class CannotReify(prod: Prod[?]) extends RuntimeException(s"cannot reify $prod")

  trait Rewrite:
    def rewrite[U](prod: Prod[U]): Prod[U]
  end Rewrite

  object Rewrite:
    def rw[T : Tag](fn: PartialFunction[Prod[T], Prod[T]]): Rewrite =
      Rewrite.PartialFunctionRewrite(fn)
    end rw

    final class PartialFunctionRewrite[T : Tag](fn: PartialFunction[Prod[T], Prod[T]]) extends Rewrite:
      def rewrite[U](prod: Prod[U]): Prod[U] =
        if prod.tag <:< Tag[T]
        then fn
          .asInstanceOf[PartialFunction[Prod[U], Prod[U]]]
          .applyOrElse(prod, identity)
        else prod
      end rewrite
    end PartialFunctionRewrite

    final class RewriteSeq(rewrites: Seq[Rewrite]) extends Rewrite:
      def rewrite[U](prod: Prod[U]): Prod[U] =
        rewrites.foldLeft(prod)((prod, rw) => rw.rewrite(prod))
      end rewrite
    end RewriteSeq
  end Rewrite

  private final class Reject[T](val tag: Tag[T], val message: String) extends Prod[T]:
    def reify(): T = throw CannotReify(this)
    protected def rewriteChildren(fn: [U] => Prod[U] => Prod[U]): Prod[T] = this
    protected def unapplyLifted[U](ops: LiftableOps[U]): Option[ops.U] = None
    def cast[U: Tag]: Prod[U] = Reject(Tag[U], message)
  end Reject
  
  private final class Lifted[T, U](val ops: LiftableOps.Aux[T, U], val u: U) extends Prod[T]:
    def tag = ops.tag
    def reify(): T = ops.reify(u)
    protected def rewriteChildren(fn: [U] => (x: Prod[U]) => Prod[U]): Prod[T] =
      val rw = ops.rewriteChildren(u, fn)
      if rw.asInstanceOf[AnyRef] ne u.asInstanceOf[AnyRef]
      then Lifted(ops, rw)
      else this
    end rewriteChildren
    protected def unapplyLifted[V](ops: LiftableOps[V]): Option[ops.U] =
      if this.ops.tag <:< ops.tag
      then Some(u.asInstanceOf[ops.U])
      else None
    end unapplyLifted
    def cast[U: Tag]: Prod[U] =
      if ops.tag =:= Tag[U]
      then this.asInstanceOf
      else if ops.tag <:< Tag[U]
      then Super(Tag[U], this.asInstanceOf)
      else Cast(Tag[U], this)
    end cast
  end Lifted

  private final class Super[T, V <: T](val tag: Tag[T], val prod: Prod[V]) extends Prod[T]:
    def reify(): T = prod.reify()
    protected def rewriteChildren(fn: [U] => Prod[U] => Prod[U]): Prod[T] =
      val rw = fn[V](prod)
      if rw ne prod
      then rw.upcast[T](using tag)
      else this
    end rewriteChildren
    export prod.unapplyLifted
    def cast[U: Tag]: Prod[U] =
      if Tag[U] == tag
      then this.asInstanceOf
      else prod.cast[U]
    end cast
  end Super

  private final class Cast[T, V](val tag: Tag[T], val prod: Prod[V]) extends Prod[T]:
    def reify(): T = throw CannotReify(this)
    protected def rewriteChildren(fn: [U] => Prod[U] => Prod[U]): Prod[T] =
      val rw = fn[V](prod)
      if rw ne prod
      then rw.cast[T](using tag)
      else this
    end rewriteChildren
    export prod.unapplyLifted
    def cast[U: Tag]: Prod[U] =
      if Tag[U] == tag
      then this.asInstanceOf
      else prod.cast[U]
    end cast
  end Cast

  trait Liftable[T]:
    def lift(t: T): Prod[T]
  end Liftable

  object Liftable:
    transparent inline def derived[T : Tag](using mirror: Mirror.Of[T])(using =>TupleOf[Tuple.Map[mirror.MirroredElemTypes, Liftable]]): Liftable[T] | LiftableWithOps.Aux[T, Tuple.Map[mirror.MirroredElemTypes, Prod]] =
      inline mirror match
        case mirror: Mirror.ProductOf[t & Product] =>
          liftableProductOf[t & Product](using mirror)
            .asInstanceOf[LiftableWithOps.Aux[T, Tuple.Map[mirror.MirroredElemTypes, Prod]]]
        case mirror: Mirror.SumOf[T] =>
          liftableSumOf[T](using mirror)
      end match
    end derived
  end Liftable

  trait LiftableOps[T]:
    type U
    def reify(u: U): T
    def rewriteChildren(u: U, fn: [u] => Prod[u] => Prod[u]): U
    given tag: Tag[T] = deferred
  end LiftableOps

  object LiftableOps:
    type Aux[T, U0] = LiftableOps[T] {
      type U = U0
    }
  end LiftableOps

  trait LiftableWithOps[T] extends Liftable[T], LiftableOps[T]

  object LiftableWithOps:
    type Aux[T, U0] = LiftableWithOps[T] {
      type U = U0
    }
  end LiftableWithOps

  trait IdentityLiftable[T] extends LiftableWithOps[T]:
    type U = T
    def lift(t: T) = Lifted(this, t)
    def reify(u: U): T = u
    def rewriteChildren(u: U, fn: [u] => Prod[u] => Prod[u]): U = u
  end IdentityLiftable

  given identityLiftableByte: IdentityLiftable[Byte] {}
  given identityLiftableInt: IdentityLiftable[Int] {}
  given identityLiftableLong: IdentityLiftable[Long] {}
  given identityLiftableShort: IdentityLiftable[Short] {}
  given identityLiftableDouble: IdentityLiftable[Double] {}
  given identityLiftableFloat: IdentityLiftable[Float] {}
  given identityLiftableString: IdentityLiftable[String] {}

  given liftableProductOf: [T <: Product : Tag] => (mirror: Mirror.ProductOf[T]) => (innerLiftables: =>TupleOf[Tuple.Map[mirror.MirroredElemTypes, Liftable]]) => LiftableWithOps.Aux[T, Tuple.Map[mirror.MirroredElemTypes, Prod]] =
    new LiftableWithOps[T]:
      type U = Tuple.Map[mirror.MirroredElemTypes, Prod]
      def lift(t: T): Prod[T] =
        val elems = t
          .productIterator
          .zip(innerLiftables.value.productIterator.asInstanceOf[Iterator[Liftable[Any]]])
          .map: (elem, liftable) =>
            liftable.lift(elem)
          .toArray
        end elems
        Lifted(this, Tuple.fromArray(elems).asInstanceOf[U])
      end lift
      def reify(u: U): T =
        mirror.fromTuple(scala.runtime.Tuples.map(u, [t] => tt => tt.asInstanceOf[Prod[Any]].reify().asInstanceOf).asInstanceOf[mirror.MirroredElemTypes])
      end reify
      def rewriteChildren(u: U, fn: [u] => Prod[u] => Prod[u]): U =
        val elems = u
          .toArray
          .mapInPlace(x => fn[Object](x.asInstanceOf[Prod[Object]]))
        end elems
        if elems.iterator.zipWithIndex.exists((p, i) => p ne u.productElement(i).asInstanceOf[Object])
        then Tuple.fromArray(elems).asInstanceOf[U]
        else u
      end rewriteChildren
    end new
  end liftableProductOf

  given liftableSumOf: [T : Tag] => (mirror: Mirror.SumOf[T]) => (innerLiftables: =>TupleOf[Tuple.Map[mirror.MirroredElemTypes, Liftable]]) => Liftable[T]:
    def lift(t: T): Prod[T] =
      innerLiftables
        .value
        .productElement(mirror.ordinal(t))
        .asInstanceOf[Liftable[T]]
        .lift(t)
        .cast[T]
    end lift
  end liftableSumOf

  given liftableList: [T] => Tag[List[T]] => (liftable: Liftable[T]) => LiftableWithOps.Aux[List[T], List[Prod[T]]] =
    new LiftableWithOps[List[T]]:
      type U = List[Prod[T]]
      def lift(t: List[T]): Prod[List[T]] = Lifted(this, t.map(elem => liftable.lift(elem)))
      def reify(u: List[Prod[T]]): List[T] = u.map(_.reify())
      def rewriteChildren(u: List[Prod[T]], fn: [u] => Prod[u] => Prod[u]): U =
        u.mapConserve(_.rewriteChildren(fn))
      end rewriteChildren
    end new
  end liftableList
end Prod
