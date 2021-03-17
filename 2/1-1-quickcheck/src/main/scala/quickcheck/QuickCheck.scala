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


}
