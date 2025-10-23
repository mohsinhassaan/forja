// Copyright 2024-2025 Forja Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package forja

import java.nio.ByteBuffer
import java.nio.channels.Channels
import java.nio.charset.{Charset, StandardCharsets}

import forja.util.MonomorphicIndexedSeq

import scala.collection.mutable

final class SourceRange(
    val source: Source,
    val offset: Int,
    val length: Int,
) extends MonomorphicIndexedSeq[Byte, SourceRange]:
  require(
    0 <= offset && 0 <= length && offset + length <= source.byteBuffer.limit(),
    s"invalid offset = $offset, length = $length into source with limit ${source.byteBuffer.limit()}",
  )

  override def toString(): String =
    s"SourceRange(${{ decodeString() }})"
  end toString

  def apply(i: Int): Byte =
    source.byteBuffer
      .get(offset + i)
  end apply

  def byteBuffer(): ByteBuffer =
    source.byteBuffer
      .duplicate()
      .position(offset)
      .limit(offset + length)
      .slice()
  end byteBuffer

  def decodeString(charset: Charset = StandardCharsets.UTF_8): String =
    charset.decode(byteBuffer()).toString()
  end decodeString

  def writeBytesTo(out: java.io.OutputStream): Unit =
    val channel = Channels.newChannel(out)
    val bufSlice = this.byteBuffer()
    // It seems .write will make a significant effort to write all bytes,
    // but just to be safe let's retry if it writes less.
    var count = 0
    while count < length
    do count += channel.write(bufSlice)
  end writeBytesTo

  def emptyAtOffset: SourceRange =
    dropRight(length)
  end emptyAtOffset

  def extendLeftBy(count: Int): SourceRange =
    SourceRange(source, offset - count, length)
  end extendLeftBy

  def extendLeft: SourceRange =
    extendLeftBy(1)
  end extendLeft

  def extendRightBy(count: Int): SourceRange =
    SourceRange(source, offset, length + count)
  end extendRightBy

  def extendRight: SourceRange =
    extendRightBy(1)
  end extendRight

  protected def sliceImpl(from: Int, until: Int): SourceRange =
    require(
      0 <= from && from <= until && until <= length,
      s"[$from, $until) is outside the range [$offset,${offset + length})",
    )
    SourceRange(source, offset + from, until - from)
  end sliceImpl

  @scala.annotation.targetName("combine")
  def <+>(other: SourceRange): SourceRange =
    if source eq Source.empty
    then other
    else if other.source eq Source.empty
    then this
    else if source eq other.source
    then
      // convert to [start,end) spans so we can do min/max merge.
      // Doesn't work on lengths because those are relative to offset,
      // but start / end are independent.
      val (startL, endL) = (offset, offset + length)
      val (startR, endR) = (other.offset, other.offset + other.length)
      val (startC, endC) = (startL.min(startR), endL.max(endR))
      SourceRange(source, startC, endC - startC)
    else this
  end <+>

  def presentationStringShort: String =
    val builder = StringBuilder()
    builder ++= source.origin.map(_.toString).getOrElse("<internal>")
    builder += ':'

    val (lineIdx, colIdx) = source.lines.lineColAtOffset(offset)
    builder.append(lineIdx + 1)
    builder += ':'
    builder.append(colIdx + 1)

    if length > 0
    then
      builder += '-'
      val (lineIdx2, colIdx2) = source.lines.lineColAtOffset(offset + length)
      if lineIdx == lineIdx2
      then builder.append(colIdx2 + 1)
      else
        builder.append(lineIdx2 + 1)
        builder += ':'
        builder.append(colIdx2 + 1)

    builder.result()
  end presentationStringShort

  def presentationStringLong: String =
    s"$presentationStringShort:\n$showInSource"
  end presentationStringLong

  def showInSource: String =
    extension (it: Iterator[Byte])
      // When we print a line, we don't want the trailing newline.
      // We also don't want platform specific variants like \r.
      def removeCrLf: Iterator[Byte] =
        it.filter(ch => ch != '\r' && ch != '\n')

      def decodeString: String =
        String(it.toArray, StandardCharsets.UTF_8)

    val entireSource = SourceRange.entire(source)
    val builder = StringBuilder()
    val (line1Idx, startCol) = source.lines.lineColAtOffset(offset)
    val (line2Idx, endCol) = source.lines.lineColAtOffset(offset + length)

    val line1Start = source.lines.lineStartOffset(line1Idx)
    val line1AfterEnd = source.lines.lineStartOffset(line1Idx + 1)

    if line1Idx == line2Idx
    then
      val lineFrag = entireSource.slice(line1Start, line1AfterEnd)
      builder.addAll(lineFrag.iterator.removeCrLf.decodeString)
      builder += '\n'
      lineFrag
        .slice(0, startCol)
        .iterator
        .removeCrLf
        .foreach(_ => builder += ' ')
      if startCol == endCol
      then builder += '^'
      else
        lineFrag
          .slice(startCol, endCol)
          .iterator
          .removeCrLf
          .foreach(_ => builder += '^')
    else
      val line1Frag = entireSource.slice(line1Start, line1AfterEnd)
      line1Frag
        .slice(0, startCol)
        .iterator
        .removeCrLf
        .foreach(_ => builder += ' ')
      val line1EndCol = line1AfterEnd - line1Start
      line1Frag
        .slice(startCol, line1EndCol)
        .iterator
        .removeCrLf
        .foreach(_ => builder += 'v')
      builder += '\n'
      builder.addAll(line1Frag.iterator.removeCrLf.decodeString)

      if line2Idx - line1Idx > 1
      then builder.addAll("\n...\n")
      else builder += '\n'

      val line2Start = source.lines.lineStartOffset(line2Idx)
      val line2AfterEnd = source.lines.lineStartOffset(line2Idx + 1)
      val line2Frag = entireSource.slice(line2Start, line2AfterEnd)
      builder.addAll(line2Frag.iterator.removeCrLf.decodeString)
      builder += '\n'
      line2Frag
        .slice(0, endCol)
        .iterator
        .removeCrLf
        .foreach(_ => builder += '^')

    builder.result()
  end showInSource
end SourceRange

object SourceRange:
  final class Builder extends mutable.Builder[Byte, SourceRange]:
    private val arrayBuilder = Array.newBuilder[Byte]
    def addOne(elem: Byte): this.type =
      arrayBuilder.addOne(elem)
      this
    def clear(): Unit = arrayBuilder.clear()
    def result(): SourceRange =
      SourceRange.entire(
        Source.fromByteBuffer(ByteBuffer.wrap(arrayBuilder.result())),
      )

  def apply(str: String): SourceRange =
    entire(Source.fromString(str))

  def apply(source: Source, offset: Int, length: Int): SourceRange =
    new SourceRange(source, offset, length)

  def newBuilder: Builder = Builder()

  def entire(source: Source): SourceRange =
    new SourceRange(source, 0, source.byteBuffer.limit())

  given embed: Node.Embed[SourceRange]:
    def extractOption(value: Matchable): Option[SourceRange] =
      value match
        case sourceRange: SourceRange => Some(sourceRange)
        case _                        => None
    end extractOption
  end embed

  extension (ctx: StringContext)
    def src: srcImpl =
      srcImpl(ctx)
    end src
  end extension

  final class srcImpl(val ctx: StringContext) extends AnyVal:
    def unapplySeq(sourceRange: SourceRange): Option[Seq[SourceRange]] =
      val parts = ctx.parts
      assert(parts.nonEmpty)

      extension (part: String)
        def bytes: SourceRange =
          SourceRange.entire:
            Source.fromByteBuffer:
              StandardCharsets.UTF_8.encode:
                StringContext.processEscapes(part)

      val firstPart = parts.head.bytes

      if !sourceRange.startsWith(firstPart)
      then return None

      var currRange = sourceRange.drop(firstPart.length)
      val matchedParts = mutable.ListBuffer.empty[SourceRange]
      val didMatchFail =
        parts.iterator
          .drop(1)
          .map(_.bytes)
          .map: part =>
            if part.isEmpty
            then
              val idx = currRange.length
              matchedParts += currRange
              currRange = currRange.drop(currRange.length)
              idx
            else
              val idx = currRange.indexOfSlice(part)
              if idx != -1
              then
                matchedParts += currRange.take(idx)
                currRange = currRange.drop(idx)

              idx
          .contains(-1)
          || currRange.nonEmpty

      if didMatchFail
      then None
      else Some(matchedParts.result())
    end unapplySeq
  end srcImpl
end SourceRange
