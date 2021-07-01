package com.cmhteixeira.typedlist.support4cats

import cats.{Applicative, Eval, Traverse}
import com.cmhteixeira.typedlist.TypedList
import com.cmhteixeira.typedlist.naturalnumbers.Natural

import scala.util.control.TailCalls.done

trait Implicits {

  implicit def catsTraverseInstance[Size <: Natural]: Traverse[TypedList[Size, *]] =
    new Traverse[TypedList[Size, *]] {

      override def traverse[G[_], A, B](fa: TypedList[Size, A])(f: A => G[B])(
          implicit evidence$1: Applicative[G]
      ): G[TypedList[Size, B]] = fa.traverseHelper(a => done(f(a))).result

      override def foldLeft[A, B](fa: TypedList[Size, A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: TypedList[Size, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        implicit def myApplicative[P]: Applicative[Lambda[A => Function[Eval[P], Eval[P]]]] =
          new Applicative[Lambda[A => Function[Eval[P], Eval[P]]]] {
            override def pure[K](x: K): Function[Eval[P], Eval[P]] = identity

            override def ap[K, L](
                ff: Function[Eval[P], Eval[P]]
            )(fa: Function[Eval[P], Eval[P]]): Function[Eval[P], Eval[P]] =
              b => Eval.defer(ff(fa(b)))
          }

        fa.traverseHelperCats[Lambda[Y => Function[Eval[B], Eval[B]]], B](a => Eval.now((bRec: Eval[B]) => f(a, bRec)))
          .flatMap(f => f(lb))
      }

    }
}
