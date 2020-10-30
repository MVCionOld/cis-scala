package homeworks.homework1

object task1 extends App {
  import scala.annotation.tailrec
  /**
   * Напишите хвосторекурсивную функцию, возвращающую n-e число из последовательности коров Нараяны,
   * задаваемой отношением
   *
   * a_0 = a_1 = a_2 = 1
   * a_n = a_{n-1} + a_{n-3}
   *
   * https://oeis.org/A000930
   *
   * @param n номер числа последовательности
   * @return n-ое число последовательности коров Нараяны (согласно формуле выше)
   */
  def narayanaCows(n: Int): Int = {
    @tailrec
    def narayanaCowsHelper(n: Int, k: Int, a3: Int, a2: Int, a1: Int): Int = k match {
      case x if x != n => narayanaCowsHelper(n, k+1, a2, a1, a1+a3)
      case x if x == n => a1+a3
    }
    n match {
      case x if x < 3 => 1
      case _ => narayanaCowsHelper(n, 3, 1, 1, 1)
    }
  }

  (for (i <- 0 until 10) yield s"$i) ${narayanaCows(i)}").foreach(println)
  //  0) 1
  //  1) 1
  //  2) 1
  //  3) 2
  //  4) 3
  //  5) 4
  //  6) 6
  //  7) 9
  //  8) 13
  //  9) 19
}
