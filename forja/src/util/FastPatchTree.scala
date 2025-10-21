package forja.util

import scala.collection.immutable.IndexedSeqOps
import scala.collection.{
  IterableFactoryDefaults,
  StrictOptimizedSeqFactory,
  mutable,
}

final class FastPatchTree[T](left: Vector[T], right: Vector[T])
    extends IndexedSeq[T],
      IndexedSeqOps[T, FastPatchTree, FastPatchTree[T]],
      IterableFactoryDefaults[T, FastPatchTree]:
  override def iterableFactory = FastPatchTree

  def apply(i: Int): T =
    if i < left.length
    then left(i)
    else right(i - left.length)
  end apply

  override def updated[B >: T](index: Int, elem: B): FastPatchTree[B] =
    if left.indices.contains(index)
    then
      new FastPatchTree(
        left = left.updated(index, elem),
        right = right,
      )
    else if right.indices.contains(index - left.length)
    then
      new FastPatchTree(
        left = left,
        right = right.updated(index - left.length, elem),
      )
    else throw IndexOutOfBoundsException(index.toString())
  end updated

  def length: Int =
    left.length + right.length

  override def slice(from: Int, until: Int): FastPatchTree[T] =
    new FastPatchTree(
      left = left.slice(from, until),
      right = right.slice(from - left.length, until - left.length),
    )
  end slice

  override def patch[B >: T](
      from: Int,
      other: IterableOnce[B],
      replaced: Int,
  ): FastPatchTree[B] =
    new FastPatchTree(
      left = left
        .take(from)
        .appendedAll(right.take(from - left.length))
        .appendedAll(other),
      right = right
        .drop(from + replaced - left.length)
        .prependedAll(left.drop(from + replaced)),
    )
  end patch

  override def take(n: Int): FastPatchTree[T] =
    slice(0, n)

  override def drop(n: Int): FastPatchTree[T] =
    slice(n, length)

  override def dropRight(n: Int): FastPatchTree[T] =
    slice(0, length - n)

  override def takeRight(n: Int): FastPatchTree[T] =
    slice(length - n, length)
end FastPatchTree

object FastPatchTree extends StrictOptimizedSeqFactory[FastPatchTree]:
  def empty[A]: FastPatchTree[A] =
    new FastPatchTree(left = Vector.empty, right = Vector.empty)
  def from[A](source: IterableOnce[A]): FastPatchTree[A] =
    new FastPatchTree(left = Vector.empty, right = Vector.from(source))
  def newBuilder[A]: mutable.Builder[A, FastPatchTree[A]] =
    new mutable.Builder[A, FastPatchTree[A]]:
      private val vectorBuilder = Vector.newBuilder[A]
      export vectorBuilder.clear
      def addOne(elem: A): this.type =
        vectorBuilder.addOne(elem)
        this
      def result(): FastPatchTree[A] =
        new FastPatchTree(left = Vector.empty, right = vectorBuilder.result())
  end newBuilder
end FastPatchTree
