package wk1

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


 @RunWith(classOf[JUnitRunner])
  class Wk1Suite extends FunSuite {


  test("intNotZero throws an exception if its argument is 0") {
    intercept[IllegalArgumentException] {
      intNotZero(0)
    }
  }

  def intNotZero(x: Int): Int = {
    if (x == 0) throw new IllegalArgumentException("zero is not allowed")
    else x
  }

  test ("sqrt of 4 is close to 2"){
      wk1.Session.sqrt(4)*wk1.Session.sqrt(4) < 0.001*4
  }

  test ("sqrt of small number"){
      wk1.Session.sqrt(1e-6)*wk1.Session.sqrt(1e-6) < 0.001*1e-6
  }

  test ("sqrt of large number"){
      wk1.Session.sqrt(1e50)*wk1.Session.sqrt(1e50) < 0.001*1e50
  }



}
