package com.cmh.teixeira.typedlist

import com.cmhteixeira.typedlist.TypedNil
import com.cmhteixeira.typedlist.naturalnumbers.Natural.{Nat1, Nat3}
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

class TypedListSpec extends AnyFunSuiteLike with Matchers {
  test("A non-empty list can be transformed into a standard list") {
    val list3Elements = "Foo" :: "Bar" :: "Baz" :: TypedNil
    list3Elements.toList shouldBe List("Foo", "Bar", "Baz")
  }

  test("An empty list should be able to be transformed into an empty standard list") {
    TypedNil.toList shouldBe List()
  }

  // should this really be here?
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

  test("Determining if all the elements verify a predicate should be supported") {
    val list4Elements = "Foo" :: "FooBar" :: "FooBaz" :: "FooQux" :: TypedNil
    list4Elements.forall(_.contains("Foo")) shouldBe true
  }

  test("It should be possible to get an element by its index") {
    ("Foo" :: "Bar" :: "Baz" :: "Qux" :: TypedNil).get[Nat3] shouldBe "Baz"
  }

  test("It should not be possible to get an element by its index") {
    ("Foo" :: "Bar" :: "Baz" :: "Qux" :: TypedNil).get[Nat3] shouldBe "Baz"
  }

  test("An empty list should have size zero") {
    (1 :: TypedNil).tail.size shouldBe 0
  }

  test("A list with 3 elements should have size 3") {
    ("Foo" :: "Bar" :: "Baz" :: TypedNil).size shouldBe 3
  }

  test("Concatenating 2 lists should result in a list with a size as big as the sum of its parts") {
    (1 :: 2 :: 3 :: TypedNil).concat(10 :: 20 :: 30 :: TypedNil).size shouldBe 6
  }

  test(
    "Concatenating 2 lists should  result in a list with the elements from the left hand operand followed by the elements of the right hand operand"
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

}
