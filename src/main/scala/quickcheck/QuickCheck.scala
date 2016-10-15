package quickcheck

import org.scalacheck.Prop._
import org.scalacheck._
import Arbitrary.arbitrary

import scala.collection.mutable.ListBuffer


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[A]
    m <- Gen.oneOf(Gen.const(empty), genHeap)
  } yield insert(v,m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert 2 elems into an empty heap then min(heap)= min(2 elem)") = forAll{(h: H)=>
    val m1 = 3
    val m2 = 5
    findMin((insert(m1,insert(m2,empty)))) == 3
  }

  property("insert into empty then delete the element should generate empty heap ") = forAll{(h: H)=>
    val m1 = 3
    isEmpty(deleteMin(insert(m1,empty)))
  }

 property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima.")
  = forAll{(h: H) =>
   val m = if (isEmpty(h)) 0 else findMin(h)
    verifySortedHeap(h,m)

  }

  def verifySortedHeap(h:H, elem: A):Boolean={
    if(isEmpty(h))
      return true
    else{
      val thsMin = findMin(h)
      val restHeap = deleteMin(h)
      return elem <= thsMin && verifySortedHeap(restHeap,thsMin)
    }
  }
  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other.") = forAll{(h1:H, h2: H)=>
    val m1 = if (isEmpty(h1)) 0 else findMin(h1)
    val m2 = if (isEmpty(h2)) 0 else findMin(h2)
    val h12 = meld(h1,h2)
    val mm = if (isEmpty(h12)) 0 else findMin(h12)

    mm == findMin(insert(m1,insert(m2,empty)))
  }

  property("testing meld - equal through min ") = forAll{(h1:H, h2:H)=>
    val hm = meld(h1,h2)
    val mm = if(isEmpty(hm)) 0 else findMin(hm)
    verifyHeapMeldEqual(meld(h1,h2),insert(mm,deleteMin(hm)))

  }

  property(" meld - equal through min/del ") = forAll{(h1:H, h2:H)=>
    val m1 = if(isEmpty(h1)) 0 else findMin(h1)
    val m2 = if(isEmpty(h2)) 0 else findMin(h2)
    if(m1< m2){
      verifyHeapMeldEqual(meld(deleteMin(h1),h2),deleteMin(meld(h1,h2)))
    }
    else{
      verifyHeapMeldEqual(meld(h1,deleteMin(h2)),deleteMin(meld(h1,h2)))
    }

  }

  def verifyHeapMeldEqual(h1:H,h2:H):Boolean = {
    if(isEmpty(h1) && isEmpty(h2))
      return true
    else if((isEmpty(h1) && !isEmpty(h2)) || (isEmpty(h2) && !isEmpty(h1)))
      return false
    else{
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      m1 == m2 && verifyHeapMeldEqual(deleteMin(h1),deleteMin(h2))
    }
  }
}
