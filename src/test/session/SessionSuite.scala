package session

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner


 @RunWith(classOf[JUnitRunner])
  class SessionSuite extends FunSuite {


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
      session.Session.sqrt(4)*session.Session.sqrt(4) < 0.001*4
  }

  test ("sqrt of small number"){
      session.Session.sqrt(1e-6)*session.Session.sqrt(1e-6) < 0.001*1e-6
  }

  test ("sqrt of large number"){
      session.Session.sqrt(1e50)*session.Session.sqrt(1e50) < 0.001*1e50
  }



}
