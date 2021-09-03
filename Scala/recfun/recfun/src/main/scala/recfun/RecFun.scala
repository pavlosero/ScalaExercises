package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }

    println(balance("())(".toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (r == 0) 0
    else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @scala.annotation.tailrec
    def balancedAcc(cc: List[Char], counter: Int): Boolean = {
      if (cc.isEmpty) counter == 0
      else if(counter < 0 ) false
      else if (cc.head == '(') balancedAcc(cc.tail, counter+1)
      else if (cc.head == ')') balancedAcc(cc.tail, counter-1)
      else balancedAcc(cc.tail, counter)
    }

    balancedAcc(chars, 0)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money<0 || (coins.isEmpty && money>=1)) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
