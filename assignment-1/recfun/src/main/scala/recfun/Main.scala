package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    //    val x = countChange(10, List(10, 5, 2))
    //    println(x)
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    // this version does not use Memoization
    // it will explore the whole recursive tree.
    def pascalV1(c: Int, r: Int): Int =
      if (c == 0 || c == r) 1
      else pascalV1(c - 1, r - 1) + pascalV1(c, r - 1)

    /* There is a problem with using the stack collection */
    //    // this version uses Memoization to reduce 
    //    // the number of recursive calls needed
    //    val memo = scala.collection.mutable.HashMap.empty[(Int, Int),  Int]
    //    def pascalV2(c: Int, r: Int): Int = {
    //      if (c == 0 || c == r) 1
    //      else {
    //        if (memo contains ((c, r))) memo((c, r))
    //        else {
    //        	val value = pascalV2(c - 1, r - 1) + pascalV2(c, r - 1)
    //        	memo += ((Tuple2(5,6), 6))
    //        }
    //      }
    //    }

    pascalV1(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    /*
    // validation using a stack to push open brackets and 
    // pop whenever a close one is found
    val stack = new scala.collection.mutable.Stack[Char]
    def validate(xs: List[Char]): Boolean = {
      if (xs.isEmpty) stack.isEmpty
      else {
        if (xs.head == '(') stack.push('(')
        else if (xs.head == ')') {
          if (stack.isEmpty) return false
          else stack.pop
        }
        validate(xs.tail)
      }
    }
    */

    // this version uses count of opening instead of a stack
    def validateV2(xs: List[Char], count: Int): Boolean = {
      if (count < 0) false // to handle )(
      else if (xs.isEmpty) count == 0
      else if (xs.head == '(') validateV2(xs.tail, count + 1)
      else if (xs.head == ')') validateV2(xs.tail, count - 1)
      else validateV2(xs.tail, count)
    }

    // validate(chars)
    validateV2(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty && money >= 1) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)

  }
}
