package com.cmhteixeira.typedlist.support4cats

import cats.{Applicative, Eval, Traverse}
import com.cmhteixeira.typedlist.naturalnumbers.Natural
import com.cmhteixeira.typedlist.naturalnumbers.Natural.Nat1
import com.cmhteixeira.typedlist.{TypedCons, TypedList, TypedNil}

trait Implicits {

  implicit def catsTraverseInstance[Size <: Natural]: Traverse[({ type alpha[A] = TypedList[Size, A] })#alpha] =
    new Traverse[({ type alpha[A] = TypedList[Size, A] })#alpha] {

      override def traverse[G[_], A, B](fa: TypedList[Size, A])(f: A => G[B])(
          implicit evidence$1: Applicative[G]
      ): G[TypedList[Size, B]] = fa match {
        case TypedNil => evidence$1.pure(TypedNil)
        case TypedCons(_head: A, _tail: TypedList[Size#Minus[Nat1], A]) =>
          evidence$1.map2(f(_head), catsTraverseInstance.traverse(_tail)(f)) {
            case (a, (b: TypedList[Size#Minus[Nat1], B])) => a :: b
          }
      }

      override def foldLeft[A, B](fa: TypedList[Size, A], b: B)(f: (B, A) => B): B = {
        val myApplicative: Applicative[({ type alpha[A] = Function[B, B] })#alpha] =
          new Applicative[({ type alpha[A] = Function[B, B] })#alpha] {
            override def pure[K](x: K): Function[B, B] = identity

            override def ap[K, L](ff: Function[B, B])(fa: Function[B, B]): Function[B, B] = b => fa(ff(b))
          }

        traverse[({ type alpha[A] = Function[B, B] })#alpha, A, B](fa)((a: A) => (b: B) => f(b, a))(myApplicative)(b)
      }

      override def foldRight[A, B](fa: TypedList[Size, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        val myApplicative: Applicative[({ type alpha[A] = Function[Eval[B], Eval[B]] })#alpha] =
          new Applicative[({ type alpha[A] = Function[Eval[B], Eval[B]] })#alpha] {
            override def pure[K](x: K): Function[Eval[B], Eval[B]] = identity

            override def ap[K, L](ff: Function[Eval[B], Eval[B]])(
                fa: Function[Eval[B], Eval[B]]
            ): Function[Eval[B], Eval[B]] = b => ff(fa(b))
          }

        traverse[({ type alpha[A] = Function[Eval[B], Eval[B]] })#alpha, A, B](fa) { (a: A) => (b: Eval[B]) =>
          f(a, b)
        }(myApplicative)(lb)
      }

    }
}
