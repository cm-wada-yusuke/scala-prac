package fp.output

/**
  * Created by y-wada on 2016/03/20.
  */
object EX22 {

  /**
    *
    * @param as      検査対象の配列。
    * @param ordered 2値をとりソート条件を満たしているかどうかを返す関数。
    * @tparam A 検査対象配列の要素型
    * @return asがソート済かどうか
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) ordered(as(n - 1), as(n)) // 終わり
      else if (as.length <= 1) true //要素がひとつ以下のときは常にtrue
      else if (ordered(as(n - 1), as(n))) loop(n + 1)
      else false
    loop(1)
  }


  /**
    * 本当はテストを書くべき。
    *
    * @param args メイン関数引数。
    */
  def main(args: Array[String]): Unit = {
    val ordered = { (x: Int, y: Int) => y >= x }
    val array = Array(1, 2, 4, 3, 6, 7, 8, 9, 10)
    println(array.mkString(", "))
    println(isSorted(array, ordered))
  }

}
