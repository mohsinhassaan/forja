package forja.util

import utest.*

final class FastPatchTreeTest extends TestSuite:
  def tests = Tests:
    test("patch") {
      val data1 = Seq(1, 2, 3)
      val data2 = Seq(4, 5, 6)
      val data3 = Seq(7, 8, 9)
      def forall(fn: (Int, Int, Int) => Unit): Unit =
        (0 until 3).foreach: from =>
          (0 until data2.length).foreach: take =>
            (0 until 3).foreach: replaced =>
              fn(from, take, replaced)
      end forall

      forall: (from, take, replaced) =>
        val fp1 = FastPatchTree(data1*).patch(from, data2.take(take), replaced)
        val d1 = data1.patch(from, data2.take(take), replaced)
        fp1.toSeq ==> d1

        forall: (from, take, replaced) =>
          fp1.patch(from, data3.take(take), replaced).toSeq ==> d1.patch(
            from,
            data3.take(take),
            replaced,
          )
    }
  end tests
end FastPatchTreeTest
