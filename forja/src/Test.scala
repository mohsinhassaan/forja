package forja

import java.lang.classfile.ClassFile
import java.lang.constant.ClassDesc
import java.lang.constant.ConstantDescs
import java.lang.constant.MethodTypeDesc
import java.lang.classfile.TypeKind
import forja.Prod.LiftableWithOps

object Test:
  enum AST derives Prod.Liftable:
    case A(x: Int)
    case B(x: String)
  end AST

  final case class B(x: String) derives Prod.Liftable

  summon[LiftableWithOps[AST.A]]

  def main(args: Array[String]): Unit =
    val x: Prod[AST] = Prod[AST.A](42)
    val y = x.fixpoint:
      Prod.Rewrite.rw[AST]:
        case Prod[AST.A](x) =>
          Prod[AST.B](x.reify().toString)
    println(y.reify())

    val bytes =
      ClassFile.of().build(ClassDesc.of("forja.dummy.Test"), { clb =>
        clb
          .withFlags(ClassFile.ACC_PUBLIC)
          .withMethod(ConstantDescs.INIT_NAME, ConstantDescs.MTD_void, ClassFile.ACC_PUBLIC, { mb =>
            mb.withCode: cob =>
              cob
                .aload(0)
                .invokespecial(ConstantDescs.CD_Object, ConstantDescs.INIT_NAME, ConstantDescs.MTD_void)
                .return_()
          })
          .withMethod("test", MethodTypeDesc.of(ConstantDescs.CD_int), ClassFile.ACC_PUBLIC + ClassFile.ACC_STATIC, { mb =>
            mb.withCode: cb =>
              cb
                .loadConstant(42)
                .return_(TypeKind.INT)
          })
      })
    end bytes

    object loader extends ClassLoader:
      val cls: Class[?] = defineClass(null, bytes, 0, bytes.length)
    end loader

    println(bytes.length)
    println(loader.cls.getMethod("test").invoke(null))
  end main
end Test
