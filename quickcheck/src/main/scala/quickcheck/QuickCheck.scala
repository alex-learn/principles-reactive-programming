package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  type RecElement = (H, List[Int])
  
  def sorted(h: H): List[Int] = sortedHelper((h, List()))._2
  
  def sortedHelper(e: RecElement): RecElement = e match {
    case (h, list) if isEmpty(h) => e
    case (h, list) => sortedHelper((deleteMin(h), list :+ findMin(h)))
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("hint1") = forAll { a: Int =>
    forAll { b: Int =>
      val h = insert(b, insert(a, empty))
      findMin(h) == (if (ord.lteq(a, b)) a else b)
    }
  }

  property("hint2") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("hint3") = {
    forAll { h: H =>
      val sortedElements = sorted(h)
      sortedElements == sortedElements.sorted
    }
  }

  property("hint4") = forAll { h1: H =>
    forAll { h2: H =>
      val minMeld = findMin(meld(h1, h2))
      (minMeld == findMin(h1)) || (minMeld == findMin(h2))
    }
  }

  property("sameElements") = forAll { l: List[Int] =>
    sorted((empty /: l)((h, i) => insert(i, h))) == l.sorted
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(empty, genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
