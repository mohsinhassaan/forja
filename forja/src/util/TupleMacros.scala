package forja.util

import scala.quoted.Type
import scala.quoted.Quotes
import scala.quoted.quotes

object TupleMacros:
  extension [Tpl <: Tuple : Type](tpl: Type[Tpl])(using Quotes)
    def toList: List[Type[?]] =
      tpl match
        case '[EmptyTuple] => Nil
        case '[hd *: tl] => Type.of[hd] :: Type.of[tl].toList
        case _ =>
          import quotes.reflect.*
          report.errorAndAbort(s"insufficiently precise Tuple type: ${TypeTree.of[Tpl].show}")
      end match
    end toList
  end extension

  extension (list: List[Type[?]])(using Quotes)
    def toTupleType: Type[? <: Tuple] =
      list.foldRight[Type[? <: Tuple]](Type.of[EmptyTuple]): (hd, tl) =>
        (tl, hd).runtimeChecked match
          case ('[type tl <: Tuple; tl], '[hd]) =>
            Type.of[hd *: tl]
        end match
    end toTupleType
  end extension
end TupleMacros
