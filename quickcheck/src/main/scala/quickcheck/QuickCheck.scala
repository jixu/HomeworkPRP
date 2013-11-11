package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("MergeThenToSortedList") = forAll { (h1: H, h2: H) =>
    toSortedList(meld(h1, h2)) == (toSortedList(h1) ::: toSortedList(h2)).sorted
  }

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[A]
    h <- oneOf(value(empty), genHeap)
  } yield insert(e,h)

  lazy val emptyHeap = empty

  def toSortedList(h: H): List[A] = h match {
    case `emptyHeap` => List()
    case _ => findMin(h) :: toSortedList(deleteMin(h))
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
