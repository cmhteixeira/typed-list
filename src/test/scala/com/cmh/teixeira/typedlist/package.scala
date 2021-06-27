package com.cmh.teixeira

import com.cmhteixeira.typedlist.TypedList
import com.cmhteixeira.typedlist.naturalnumbers.Natural
import org.scalacheck.{Arbitrary, Gen}

package object typedlist {

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
}
