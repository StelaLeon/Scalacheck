import scala.collection.mutable.ListBuffer

def verifySortedList(elem:Int,sequence: ListBuffer[Int]): Boolean ={
  if(sequence.isEmpty)
    return true
  else {
    val f1 =elem <= sequence.head
    val f2 = verifySortedList(sequence.head,sequence.tail)
    println(s"for elem: ${elem} got: $f1 && $f2 -----")
    return f1 && f2
  }
}

val testList = ListBuffer(2,3,4,6)

verifySortedList(testList.head,testList.tail)