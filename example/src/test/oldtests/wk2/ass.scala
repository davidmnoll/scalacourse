package wk1

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


 @RunWith(classOf[JUnitRunner])
  class Ass extends FunSuite {


  test("intNotZero throws an exception if its argument is 0") {
    intercept[IllegalArgumentException] {
      intNotZero(0)
    }
  }

  def intNotZero(x: Int): Int = {
    if (x == 0) throw new IllegalArgumentException("zero is not allowed")
    else x
  }

    test ("pascal test 1"){
        assert(wk2.Assignment.pascal(0,2)===1)
    }
    test ("pascal test 2"){
        assert(wk2.Assignment.pascal(1,2)===1)
    }
    test ("pascal test 3"){
        assert(wk2.Assignment.pascal(2,4)===3)
    }
    test ("pascal test 4"){
        assert(wk2.Assignment.pascal(2,5)===6)
    }


    test ("balance test 1"){
        assert(wk2.Assignment.balance("(if zero? x) max(/ 1 x)".toList)===true)
    }
    test ("balance test 2"){
        assert(wk2.Assignment.balance("I told him (that it's not (yet) done). (But he wasn't listening)".toList)===true)
    }
    test ("balance test 3"){
        assert(wk2.Assignment.balance(":-)".toList)===false)
    }
    test ("balance test 4"){
        assert(wk2.Assignment.balance("())(".toList)===false)
    }

    test ("countChange test 1"){
        assert(wk2.Assignment.countChange(4,List(1,2))===3)
    }
    test ("countChange test 2"){
        assert(wk2.Assignment.countChange(10,List(1,7))===2)
    }
    test ("countChange test 3"){
        assert(wk2.Assignment.countChange(9,List(1,3,7))===5)
    }



}
