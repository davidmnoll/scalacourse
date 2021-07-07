package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap  {

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

  property("exists1") = forAll { ( a: Int, h: H) =>
    val h1 = insert(a, h)
    def exists(a: Int, h: H): Boolean = {
      if(isEmpty(h)) false
      else
        if (a == findMin(h)) true
        else exists(a, deleteMin(h))
    }
    exists(a, h1)
  }


//  property("exists2") = forAll { ( a: Int, b: Int, h: H )  =>
//    val h1 = insert(a, h)
//    def existsBefore(a: Int, b: Int, h: H, foundA: Boolean): Boolean = {
//      if (!foundA){
//        if(isEmpty(h)) false
//        else
//          if (a == findMin(h)) true
//          else existsBefore(a, b, deleteMin(h))
//      }
//      if(a <= b) existsBefore(a, b, insert(b, insert(a, h)))
//      else existsBefore(b, a, insert(a, insert(b, h)))
//    }
//  }


  property("delmin") = forAll { ( a: Int )  =>
    val h =  insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("gen2") = forAll{ (h1: H, h2: H) =>
      val correctVal = if( h1.equals(empty) )
        if ( h2.equals(Nil) ) Nil
        else findMin(h2)
      else
        if (h2.equals(Nil)) findMin(h1)
        else {
          val min1 = findMin(h1)
          val min2 = findMin(h2)
          if (min1 <= min2 ) min1 else min2
        }
      val meldHeap = meld(h1, h2)
      val checkVal = if(meldHeap.equals(Nil)) Nil else findMin(meldHeap)
      correctVal == checkVal
  }

  property("sorted") = forAll { (h: H) =>
    def nextIsGreater(h: H): Boolean = {
      if (isEmpty(h)) true
      else {
        val withoutMin = deleteMin(h)
        if (isEmpty(withoutMin)) true
        else {
          val min = findMin(h)
          val nextMin = findMin(withoutMin)
          nextMin >= min && nextIsGreater(withoutMin)
        }
      }
    }
    nextIsGreater(h)
  }


}

object Main extends App {
// TODO: rewrite below func to test logic that lists are sorted
//  def nextIsGreater(h: H): Boolean = {
//    if (isEmpty(h)) true
//    else {
//      val withoutMin = deleteMin(h)
//      if (isEmpty(withoutMin)) true
//      else {
//        val min = findMin(h)
//        val nextMin = findMin(withoutMin)
//        nextMin >= min && nextIsGreater(withoutMin)
//      }
//    }
//  }

}
