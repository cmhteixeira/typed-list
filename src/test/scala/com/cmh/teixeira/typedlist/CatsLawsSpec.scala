package com.cmh.teixeira.typedlist

import cats.Eq
import cats.laws.discipline.{FoldableTests, FunctorTests}
import com.cmhteixeira.typedlist.TypedList
import com.cmhteixeira.typedlist.naturalnumbers.Natural
import com.cmhteixeira.typedlist.naturalnumbers.Natural._
import org.scalacheck.Arbitrary.{arbBitSet, arbInt, arbOption}
import org.scalacheck.{Arbitrary, Gen}
import CatsLawsSpec._
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import cats.implicits._

class CatsLawsSpec
    extends FunSuiteDiscipline
    with AnyFunSuiteLike
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
    with Matchers {
  implicit val intGen: Gen[Int] = arbInt.arbitrary
  implicit val setGen = arbBitSet.arbitrary
  implicit def intGewn[A: Arbitrary] = arbOption[A].arbitrary

  checkAll("TenElementList.FunctorLaws", FunctorTests[TypedList[Nat10, *]].functor[Int, Int, Int])

  checkAll("TenElementList.FoldableLaws", FoldableTests[TypedList[Nat10, *]].foldable[Int, Int])
}

object CatsLawsSpec {

  implicit def eqInstance[Size <: Natural, A]: Eq[TypedList[Size, A]] = new Eq[TypedList[Size, A]] {
    override def eqv(x: TypedList[Size, A], y: TypedList[Size, A]): Boolean = x == y
  }
}
