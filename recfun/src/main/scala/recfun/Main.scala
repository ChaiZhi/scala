package recfun


object Main {
  def main(args: Array[String]) {

    def product(f: Int => Int)( a: Int, b: Int): Int = {
      if (a > b) 1 else f(a) * product(f)( a + 1, b)
    }

    println(product(x => x * x)( 1, 3))

    def fact(x:Int): Int = {
      product(x => x)(1, x)
    }

    println(fact(5))



    //    println("Pascal's Triangle")
    //    for (row <- 0 to 10) {
    //      for (col <- 0 to row)
    //        print(pascal(col, row) + " ")
    //      println()
    //    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c >= r || c <= 0) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], count: Int): Boolean = {
      if (chars.isEmpty) {
        count == 0
      } else {
        if (count < 0) {
          false
        } else {
          if ('('.equals(chars.head)) {
            balanceIter(chars.tail, count + 1)
          } else if (')'.equals(chars.head)) {
            balanceIter(chars.tail, count - 1)
          } else {
            balanceIter(chars.tail, count)
          }
        }
      }
    }
    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIter(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) {
        0
      } else if (money == 0) {
        1
      } else if (money < 0) {
        0
      } else {
        countChangeIter(money - coins.head, coins) + countChangeIter(money, coins.tail)
      }
    }

    countChangeIter(money, coins)

  }
}
