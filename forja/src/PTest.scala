package forja

object PTest:
  final case class Foo(x: Int, y: String) derives P.Meta

  enum Bar derives P.Meta:
    case Ping
    case Pong(x: Int, y: P[Foo])
  end Bar

  def main(args: Array[String]): Unit =
    println(summon[P.Meta[Foo]].erase(Foo(42, "43")).rewriteInner(identity))
    val foo: P[Foo] = Foo(43, "44")
    println(foo)

    val ping: P[Bar.Ping.type] = Bar.Ping
    val pong: P[Bar.Pong] = Bar.Pong(44, foo)
    println(ping)
    println(pong)
  end main
end PTest
