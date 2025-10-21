package forja

import scala.language.experimental.into


import scala.collection.concurrent

import Wf.*

trait Wf:
  export Wf.Tok

  private val dedupRegistry = concurrent.TrieMap[SrcId, TokObj]()
  extension (tok: =>Tok)
    protected def apply(using srcId: SrcId)(): TokObj =
      dedupRegistry.getOrElseUpdate(srcId, tok)
  end extension 
end Wf

object Wf:
  
  type Tok = Conversion.into[TokObj]

  case class SrcId(file: String, line: Int)
  object SrcId:
    given instance: (file: sourcecode.File, line: sourcecode.Line) => SrcId =
      SrcId(file.value, line.value)
  end SrcId

  class TokObj(fields: List[TokObj], attrs: Map[Token, TokObj]):
  end TokObj
  object TokObj:
    given fromUnit: Conversion[Unit, TokObj]:
      def apply(x: Unit): TokObj = TokObj(Nil, Map.empty)
  end TokObj

end Wf
