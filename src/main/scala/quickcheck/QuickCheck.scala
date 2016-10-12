package quickcheck

import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = ???
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of an empty string is ") = forAll{(h: H)=>
    val m = if (isEmpty(h)){
      //insert(m,h)
      findMin(insert(m,h)) == m
    }
  }

}
