package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(this.empty),
    for {
      v <- arbitrary[A]
      h <- arbitrary[H]
    } yield this.insert(v, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { ( a: Int, b: Int )  =>
    val h =  insert(a, insert(b, empty))
    findMin(h) == ( if (a < b) a else b)
  }

  property("delmin") = forAll { ( a: Int )  =>
    val h =  insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("gen2") = forAll{ (h1: H, h2: H) =>
      val correctVal = if( isEmpty(h1) )
        if ( isEmpty(h2) ) empty
        else findMin(h2)
      else
        if (isEmpty(h2)) findMin(h1)
        else {
          val min1 = findMin(h1)
          val min2 = findMin(h2)
          if (min1 < min2 ) min1 else min2
        }
      val meldHeap = meld(h1, h2)
      val check1 = findMin(meldHeap)
      if(isEmpty(meldHeap)) correctVal.equals(empty)
      else findMin(meldHeap).equals(correctVal)
  }

  property("sorted") = forAll{ (h: H) =>
    def nextIsGreater(h:H, lastVal:A, passed:Boolean): Boolean = {
        if (isEmpty(h))
          passed
        else passed && {
            nextIsGreater(deleteMin(h), findMin(h), passed && findMin(h) >= lastVal)
        }
    }
    if (isEmpty(h)) true
    else {
      val start = findMin(h)
      nextIsGreater(h, start, true)
    }
  }

}
