package com.cmh.teixeira.typedlist

import com.cmhteixeira.typedlist.{TypedCons, TypedList, TypedNil}
import com.cmhteixeira.typedlist.naturalnumbers.Natural.{Nat1, Nat0, Nat10, Nat11, Nat2, Nat3, Nat5, Nat6, Nat4, Nat7}
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TypedListSpec extends AnyFunSuiteLike with Matchers with ScalaCheckPropertyChecks {
  test("A non-empty list can be transformed into a standard list") {
    val list3Elements = "Foo" :: "Bar" :: "Baz" :: TypedNil
    list3Elements.toList shouldBe List("Foo", "Bar", "Baz")
  }

  test("An empty list can be transformed into an empty standard list") {
    TypedNil.toList shouldBe List()
  }

  test("Mapping should be supported") {
    val list3Elements = "Foo" :: "Bar" :: "Baz" :: TypedNil
    list3Elements.map(_.length).toList shouldBe List(3, 3, 3)
  }

  test("Finding the first element that verifies a predicate should be supported") {
    val list4Elements = "Foo" :: "FooBar" :: "Baz" :: "Qux" :: TypedNil
    list4Elements.collectFirst {
      case a if a.length > 3 => a
    } shouldBe Some("FooBar")
  }

  test("For an empty list, .collectFirst should return None") {
    val list1Element = "Foo" :: TypedNil
    list1Element.tail().collectFirst { case a => a } shouldBe None
  }

  test("If a non-empty list does no contain an element for which the partial function is defined, None is returned") {
    val list4Elements = "Foo" :: "FooBar" :: "Baz" :: "Qux" :: TypedNil
    list4Elements.collectFirst {
      case a if a.length > 6 => a
    } shouldBe None
  }

  test("Determining if all the elements verify a predicate should be supported") {
    val list4Elements = "Foo" :: "FooBar" :: "FooBaz" :: "FooQux" :: TypedNil
    list4Elements.forall(_.contains("Foo")) shouldBe true
  }

  test("Counting the elements that verify a predicate should be supported") {
    import org.scalacheck.Arbitrary.arbString
    implicit val ev = arbString.arbitrary.suchThat(_.nonEmpty)
    forAll { (list: TypedList[Nat10, String], prefix: String) =>
      list.map(elem => s"$prefix$elem").count(_.contains(prefix)) shouldBe 10
    }
  }

  test("If non of the elements verify the predicate, .count should return 0") {
    import org.scalacheck.Arbitrary.arbString
    implicit val ev = arbString.arbitrary.suchThat(_.nonEmpty) // non-empty part doesn't seem to be working
    forAll { (list: TypedList[Nat10, String]) =>
      list.map(elem => s"FooBar").count(_.contains("FooBaz")) shouldBe 0
    }
  }

  test("Counting the elements that verify a predicate on an empty list should be zero") {
    import org.scalacheck.Arbitrary.arbString
    implicit val ev = arbString.arbitrary
    forAll { (list: TypedList[Nat0, String], prefix: String) =>
      list.map(elem => s"$prefix$elem").count(_.contains(prefix)) shouldBe 0
    }
  }

  test("It should be possible to get an element by its index") {
    ("Foo" :: "Bar" :: "Baz" :: "Qux" :: TypedNil).get[Nat3] shouldBe "Baz"
  }

  test("An empty list should have size zero") {
    (1 :: TypedNil).tail().size shouldBe 0
  }

  test("A list with 3 elements should have size 3") {
    ("Foo" :: "Bar" :: "Baz" :: TypedNil).size shouldBe 3
  }

  test("Concatenating 2 lists should result in a list with a size as big as the sum of its parts") {
    (1 :: 2 :: 3 :: TypedNil).concat(10 :: 20 :: 30 :: TypedNil).size shouldBe 6
  }

  test(
    "Concatenating 2 lists should result in a list with the elements from the left hand operand followed by the elements of the right hand operand"
  ) {
    ("Foo" :: "Bar" :: "Baz" :: TypedNil).concat("Foo-2" :: "Bar-2" :: "Baz-2" :: TypedNil) shouldBe
      "Foo" :: "Bar" :: "Baz" :: "Foo-2" :: "Bar-2" :: "Baz-2" :: TypedNil
  }

  test("Reversing a list should return a new list with the elements in reversed order") {
    ("Foo" :: "Bar" :: "Baz" :: TypedNil).reverse shouldBe "Baz" :: "Bar" :: "Foo" :: TypedNil
  }

  test(
    "Zipping two lists should return a new list, containing tuples referring to the elements of each list at a given index"
  ) {
    ("Foo" :: "Bar" :: "Baz" :: TypedNil).zip(1 :: 2 :: 3 :: TypedNil) shouldBe
      (("Foo", 1) :: ("Bar", 2) :: ("Baz", 3) :: TypedNil)
  }

  test(
    "Splitting a list should return a tuple of 2 lists, the first of which containing all elements up to and including the index at which the split is done"
  ) {
    val (left, right) = ("Foo" :: "Bar" :: "Baz" :: TypedNil).split[Nat1]
    left shouldBe "Foo" :: TypedNil
    right shouldBe "Bar" :: "Baz" :: TypedNil
  }

  test("Mapping can be obtained from FlatMap") {
    import org.scalacheck.Arbitrary.arbString
    implicit val ev = arbString.arbitrary
    forAll { (list: TypedList[Nat10, String]) =>
      list.map(_.length) shouldBe list.flatMap(i => TypedCons(i.length, TypedNil))
    }
  }

  test("Reverting the list twice should five the original list") {
    import org.scalacheck.Arbitrary.arbInt
    implicit val ev = arbInt.arbitrary
    forAll { (list: TypedList[Nat10#Mult[Nat2], Int]) =>
      list.reverse.reverse shouldBe list
    }
  }

  test("Concatenating two lists and then splitting at appropriate index should give the original lists") {
    import org.scalacheck.Arbitrary.arbInt
    implicit val ev = arbInt.arbitrary
    forAll { (listL: TypedList[Nat5, Int], listR: TypedList[Nat11, Int]) =>
      val (left, right) = (listL concat listR).split[Nat5]
      left shouldBe listL
      right shouldBe listR
    }
  }

  test("It is possible to create a typed list from a standard list") {
    TypedList.fromList[Nat3, Int](1 :: 2 :: 3 :: Nil) shouldBe Some(1 :: 2 :: 3 :: TypedNil)
  }

  test("Its not possible to create a typed list from a standard list with fewer elements") {
    import org.scalacheck.Arbitrary.arbInt
    import org.scalacheck.Gen.listOfN

    forAll(listOfN(3, arbInt.arbitrary)) { list =>
      TypedList.fromList[Nat4, Int](list) shouldBe None
    }
  }

  test("Method 'lastOption' returns None for an empty list") {
    import org.scalacheck.Arbitrary.arbInt
    implicit val ev = arbInt.arbitrary

    forAll { list: TypedList[Nat1, Int] =>
      list.tail().lastOption shouldBe None
    }
  }

  test("Method 'lastOption' returns the last element of a non-empty list") {
    import org.scalacheck.Arbitrary.arbInt
    implicit val ev = arbInt.arbitrary

    forAll { (list: TypedList[Nat5, Int], someInt: Int) =>
      (someInt :: list).reverse.lastOption shouldBe Some(someInt)
    }
  }

  test("Method 'headOption' returns None for an empty list") {
    import org.scalacheck.Arbitrary.arbInt
    implicit val ev = arbInt.arbitrary

    forAll { (list: TypedList[Nat1, Int], someInt: Int) =>
      (someInt :: list).headOption shouldBe Some(someInt)
    }
  }

  test("It should be possible to update the head of an non-empty list") {
    import org.scalacheck.Arbitrary.arbInt
    implicit val ev = arbInt.arbitrary

    forAll { (list: TypedList[Nat5, Int], someInt: Int) =>
      list.updated[Nat1, Int](someInt) shouldBe TypedCons(someInt, list.tail())
    }
  }

  test("It should be possible to update the last element of an non-empty list") {
    import org.scalacheck.Arbitrary.arbInt
    implicit val ev = arbInt.arbitrary

    forAll { (list: TypedList[Nat5, Int], someInt: Int) =>
      list.updated[Nat5, Int](someInt) shouldBe TypedCons(someInt, list.reverse.tail()).reverse
    }
  }

  test("Updating the element at an index and then 'getting' the element at that index should be consistent") {
    import org.scalacheck.Arbitrary.arbInt
    implicit val ev = arbInt.arbitrary

    forAll { (list: TypedList[Nat5, Int], someInt: Int) =>
      list.updated[Nat3, Int](someInt).get[Nat3] shouldBe someInt
    }
  }

  test("The string representation of an empty list is 'TypedList()'") {
    (1 :: TypedNil).tail().toString shouldBe "TypedList()"
  }

  test(
    "The string representation of an non-empty list is similar to the one for a standard list, but with prefix 'TypedList' instead of 'List'"
  ) {
    import org.scalacheck.Arbitrary.arbInt
    implicit val ev = arbInt.arbitrary

    forAll { typedList: TypedList[Nat11, Int] =>
      typedList.toString.replace("TypedList", "List") shouldBe typedList.toList.toString()
    }
  }

  test("For associative operations, .foldLeft should be the same as .foldRight") {
    import org.scalacheck.Arbitrary.arbInt
    implicit val ev = arbInt.arbitrary

    forAll { typedList: TypedList[Nat11#Mult[Nat2], Int] =>
      typedList.foldLeft(0)(_ + _) shouldBe typedList.foldRight(0)(_ + _)
    }
  }

  test(".foldLeft gives the same result as .foldLeft on a standard list") {
    import org.scalacheck.Arbitrary.arbInt
    implicit val ev = arbInt.arbitrary

    val standardList = List("a", "b", "c", "d", "f", "g")
    val list = TypedList.fromList[Nat6, String](standardList).get

    list.foldLeft("")(_ + _) shouldBe standardList.foldLeft("")(_ + _)
  }

  test(".foldRight gives the same result as .foldRight on a standard list") {
    import org.scalacheck.Arbitrary.arbInt
    implicit val ev = arbInt.arbitrary

    val standardList = List("a", "b", "c", "d", "f", "g")
    val list = TypedList.fromList[Nat6, String](standardList).get

    list.foldRight("")(_ + _) shouldBe standardList.foldRight("")(_ + _)
  }

}
