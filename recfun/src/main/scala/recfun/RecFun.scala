package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || (r == c))
      1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def rec(chars: List[Char], acc: Int): Int = {
      chars match {
        case Nil => acc
        case '(' :: rest => rec(rest, acc+1)
        case ')' :: rest => if (acc > 0) rec(rest, acc-1) else -1
        case _::rest => rec(rest, acc)
      }
    }
    rec(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def rec(money: Int, coins: List[Int]): Int = {
      if (money <= 0 || coins.isEmpty)
        0
      else if (coins.head == money)
        1
      else
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
    rec(money, coins.sorted)
  }
}
