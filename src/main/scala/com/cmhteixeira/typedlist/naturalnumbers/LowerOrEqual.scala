package com.cmhteixeira.typedlist.naturalnumbers

import scala.annotation.implicitNotFound

@implicitNotFound("${N} is not lower or equal to ${M}")
sealed trait LowerOrEqual[N <: Natural, M <: Natural] {

  def diff1(implicit theDiff: M#Minus[N]): M#Minus[N] =
    theDiff /*client of this method must be explicit about return type.*/
  def diff2: Natural
}

object LowerOrEqual {

  implicit def foo[N <: Natural, M <: Natural](implicit ev: LowerOrEqual[N, M]): LowerOrEqual[Suc[N], Suc[M]] =
    new LowerOrEqual[Suc[N], Suc[M]] {
      override def diff2: Natural = ev.diff2
    }

  implicit def bar[M <: Natural](implicit m: M): LowerOrEqual[Zero.type, M] = new LowerOrEqual[Zero.type, M] {
    override def diff2: Natural = m
  }
}
