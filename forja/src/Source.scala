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
import java.nio.channels.FileChannel
import java.nio.channels.FileChannel.MapMode
import java.nio.charset.StandardCharsets

import scala.util.Using

trait Source:
  def origin: Option[os.Path]
  def byteBuffer: ByteBuffer

  object lines:
    val nlOffsets: IArray[Int] =
      IArray.from:
        SourceRange
          .entire(Source.this)
          .iterator
          .zipWithIndex
          .collect:
            case ('\n', idx) => idx
    end nlOffsets

    def lineColAtOffset(offset: Int): (Int, Int) =
      import scala.collection.Searching.*
      val lineIdx =
        nlOffsets.search(offset) match
          case Found(foundIndex)              => foundIndex
          case InsertionPoint(insertionPoint) => insertionPoint
      val colIdx =
        if lineIdx == 0
        then offset
        else offset - 1 - nlOffsets(lineIdx - 1)

      (lineIdx, colIdx)
    end lineColAtOffset

    def lineStartOffset(lineIdx: Int): Int =
      if lineIdx == 0
      then 0
      else if lineIdx - 1 == nlOffsets.length
      then SourceRange.entire(Source.this).length
      else nlOffsets(lineIdx - 1) + 1
    end lineStartOffset
  end lines
end Source

object Source:
  object empty extends Source:
    def origin: Option[os.Path] = None
    val byteBuffer: ByteBuffer =
      ByteBuffer.wrap(IArray.emptyByteIArray.unsafeArray)
  end empty

  def fromString(string: String): Source =
    StringSource(string)
  end fromString

  def fromByteBuffer(byteBuffer: ByteBuffer): Source =
    ByteBufferSource(None, byteBuffer)
  end fromByteBuffer

  def fromWritable(writable: geny.Writable): Source =
    val out = java.io.ByteArrayOutputStream()
    writable.writeBytesTo(out)
    fromByteBuffer(ByteBuffer.wrap(out.toByteArray()))
  end fromWritable

  def mapFromFile(path: os.Path): Source =
    Using.resource(FileChannel.open(path.toNIO)): channel =>
      val mappedBuf = channel.map(MapMode.READ_ONLY, 0, channel.size())
      ByteBufferSource(Some(path), mappedBuf)
  end mapFromFile

  final class StringSource(val string: String) extends Source:
    def origin: Option[os.Path] = None
    lazy val byteBuffer: ByteBuffer =
      StandardCharsets.UTF_8.encode(string)
  end StringSource

  final class ByteBufferSource(
      val origin: Option[os.Path],
      override val byteBuffer: ByteBuffer,
  ) extends Source
end Source
