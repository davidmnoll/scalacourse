package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  def pascal(c: Int, r: Int): Int = {
      if (c >= r || c < 0 ){ 0 }
      else if( c == 0 || c == r-1){ 1 }
      else pascal(c-1, r-1) + pascal(c, r-1)
  }

  def balance(chars: List[Char]): Boolean = {

      def iterChars(subChars: List[Char], balance: Int): Int = {
          if ( balance < 0 ) balance
          else if ( subChars.isEmpty ) balance
          else if ( subChars.head == '(' ) iterChars(subChars.tail, balance + 1)
          else if ( subChars.head == ')' ) iterChars(subChars.tail, balance - 1)
          else iterChars(subChars.tail, balance)
      }
      iterChars(chars, 0) == 0
  }

  def countChange(money: Int, coins: List[Int]): Int = {

      def countChangeCombos(newMoney: Int, newCoins: List[Int],  count: Int): Int = {

          if (newCoins.isEmpty || newMoney < 0){
              0
          }
          else if (newMoney == 0){
              1
          }
          else {

              countChangeCombos(
                  newMoney,
                  newCoins.tail,
                  count
                  ) +
              countChangeCombos(
                  newMoney-newCoins.head,
                  newCoins,
                  count
                  )

          }

      }

      countChangeCombos(money, coins, 0)
  }

}
