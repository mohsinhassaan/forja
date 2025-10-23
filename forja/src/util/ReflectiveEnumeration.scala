package forja.util

import java.lang.reflect.Modifier

import scala.jdk.CollectionConverters.*
import scala.reflect.ClassTag

import ReflectiveEnumeration.*

transparent trait ReflectiveEnumeration[T <: Enumerable: ClassTag]:
  lazy val values =
    val tClass = summon[ClassTag[T]].runtimeClass
    val fields = getClass()
      .getDeclaredFields()
      .view
      .filter(field => field.getName() != "MODULE$") // skip self ptr
      .filter(field => (field.getModifiers() & Modifier.PUBLIC) != 0)
      .filter(field => tClass.isAssignableFrom(field.getType()))
      .map: field =>
        field.get(this) match
          case null =>
            // If the field is null, try to rustle it into existence via MODULE$ field on its class.
            field.getType().getField("MODULE$").get(null).asInstanceOf[T]
          case value =>
            value.asInstanceOf[T]
    val methods = getClass()
      .getDeclaredMethods()
      .view
      .filter(method => (method.getModifiers() & Modifier.PUBLIC) != 0)
      .filter(method => method.getParameterCount() == 0)
      .filter(method => tClass.isAssignableFrom(method.getReturnType()))
      .map(_.invoke(this).asInstanceOf[T])

    (fields ++ methods)
      .map: obj =>
        obj.stackEntries.find(entry =>
          getClass().isAssignableFrom(entry.getDeclaringClass()),
        ) match
          case None        => throw RuntimeException("???")
          case Some(entry) => (entry.getLineNumber(), obj)
      .toArray
      .sortInPlaceBy(_._1)
      .view
      .map(_._2)
      .toList
  end values
end ReflectiveEnumeration

object ReflectiveEnumeration:
  trait Enumerable(using line: sourcecode.Line):
    private[ReflectiveEnumeration] val stackEntries = StackWalker
      .getInstance(StackWalker.Option.RETAIN_CLASS_REFERENCE)
      .walk: stream =>
        stream
          .filter: rec =>
            classOf[ReflectiveEnumeration[?]]
              .isAssignableFrom(rec.getDeclaringClass())
          .toList()
          .asScala
          .toList
    end stackEntries
  end Enumerable
end ReflectiveEnumeration
