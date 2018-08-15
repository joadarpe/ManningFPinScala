package chapter3

import org.scalatest.{FreeSpec, Matchers}

class TreeTest extends FreeSpec with Matchers {

  val l = Leaf(1)
  val b_l_l = Branch(Leaf(1), Leaf(2))
  val b_l_b_l_l = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))

  val f: Int => String = (a: Int) => a.toString

  "Exercise 3.25 size" in {
    Tree.size(l) should be(1)
    Tree.size(b_l_l) should be(3)
    Tree.size(b_l_b_l_l) should be(5)
  }

  "Exercise 3.26 maximum" in {
    Tree.maximum(l) should be(1)
    Tree.maximum(b_l_l) should be(2)
    Tree.maximum(b_l_b_l_l) should be(3)
    Tree.maximum(Branch(Leaf(5), b_l_l)) should be(5)
  }

  "Exercise 3.27 depth" in {
    Tree.depth(l) should be(0)
    Tree.depth(b_l_l) should be(1)
    Tree.depth(b_l_b_l_l) should be(2)
    Tree.depth(Branch(Leaf(0), b_l_b_l_l)) should be(3)
  }

  "Exercise 3.28 map" in {
    Tree.map(l)(f) should be(Leaf("1"))
    Tree.map(b_l_l)(f) should be(Branch(Leaf("1"), Leaf("2")))
    Tree.map(b_l_b_l_l)(f) should be(Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3"))))
  }

  "Exercise 3.29 size" in {
    Tree.sizeF(l) should be(1)
    Tree.sizeF(b_l_l) should be(3)
    Tree.sizeF(b_l_b_l_l) should be(5)
  }

  "Exercise 3.29 maximum" in {
    Tree.maximumF(l) should be(1)
    Tree.maximumF(b_l_l) should be(2)
    Tree.maximumF(b_l_b_l_l) should be(3)
    Tree.maximumF(Branch(Leaf(5), b_l_l)) should be(5)
  }

  "Exercise 3.29 depth" in {
    Tree.depthF(l) should be(0)
    Tree.depthF(b_l_l) should be(1)
    Tree.depthF(b_l_b_l_l) should be(2)
    Tree.depthF(Branch(Leaf(0), b_l_b_l_l)) should be(3)
  }

  "Exercise 3.29 map" in {
    Tree.mapF(l)(f) should be(Leaf("1"))
    Tree.mapF(b_l_l)(f) should be(Branch(Leaf("1"), Leaf("2")))
    Tree.mapF(b_l_b_l_l)(f) should be(Branch(Leaf("1"), Branch(Leaf("2"), Leaf("3"))))
  }

}
