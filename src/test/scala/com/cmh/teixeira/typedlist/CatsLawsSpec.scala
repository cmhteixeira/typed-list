package com.cmh.teixeira.typedlist

import cats.{Eq, Functor}
import cats.laws.discipline.{FoldableTests, FunctorTests, TraverseTests}
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
  type Size10Lists[A] = TypedList[Nat10, A]
  implicit val intGen: Gen[Int] = arbInt.arbitrary
  implicit val setGen = arbBitSet.arbitrary
  implicit def intGewn[A: Arbitrary] = arbOption[A].arbitrary

  checkAll("TenElementList.FunctorLaws", FunctorTests[Size10Lists].functor[Int, Int, Int])

  checkAll("TenElementList.FoldableLaws", FoldableTests[Size10Lists].foldable[Int, Int])
}

object CatsLawsSpec {

  implicit def arbitrary[Size <: Natural, A: Gen](
      implicit size: Size,
      list: TypedList[Size, Natural]
  ): Arbitrary[TypedList[Size, A]] =
    Arbitrary(for {
      normalList <- Gen.listOfN(size.toInt, implicitly[Gen[A]])
      typedList <- TypedList.fromList[Size, A](normalList) match {
        case Some(value) => Gen.const(value)
        case None => Gen.fail
      }
    } yield typedList)

  implicit def eqInstance[Size <: Natural, A]: Eq[TypedList[Size, A]] = new Eq[TypedList[Size, A]] {
    override def eqv(x: TypedList[Size, A], y: TypedList[Size, A]): Boolean = x == y
  }
}
