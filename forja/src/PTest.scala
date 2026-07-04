package forja

object PTest:
  final case class Foo(x: Int, y: String) derives P.Meta

  def main(args: Array[String]): Unit =
    println(summon[P.Meta[Foo]].erase(Foo(42, "43")).rewriteInner(identity))
    val foo: P[Foo] = Foo(43, "44")
    println(foo)
  end main
end PTest
