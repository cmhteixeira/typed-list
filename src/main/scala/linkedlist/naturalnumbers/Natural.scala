package linkedlist.naturalnumbers

import language.higherKinds

/**
  *  The natural numbers on the type system. Defined via type recursion.
  *  Contains also the value definitions.
  *
  *  Operations defined on the types: summation, subtraction, and multiplication.
  *  Operations defined on the values: summation, and subtraction.
  *
  */
sealed trait Natural {
  type Plus[That <: Natural] <: Natural
  type Mult2[M <: Natural] <: Natural
  type Mult[M <: Natural] <: Natural

  type Previous <: Natural
  type Minus[M <: Natural] <: Natural
  type _Minus[M <: Natural] <: Natural

  // Methods
  def plus[M <: Natural](m: M): Plus[M]

  def previous: Previous
  def minus[M <: Natural](m: M): Minus[M]
  def _minus[M <: Natural](m: M): _Minus[M]

  def toInt: Int

  final override def toString: String = toInt.toString
}

case object Zero extends Natural {
  override type Plus[That <: Natural] = That
  override type Mult2[M <: Natural] = Zero.type
  override type Mult[M <: Natural] = Zero.type

  override type Previous = Zero.type
  override type Minus[M <: Natural] = Zero.type
  override type _Minus[M <: Natural] = M

  // Methods
  override def plus[M <: Natural](m: M): M = m

  override def previous: Zero.type = this
  override def minus[M <: Natural](m: M): Zero.type = this
  override def _minus[M <: Natural](m: M): M = m

  override def toInt: Int = 0
}

case class Suc[N <: Natural](n: N) extends Natural {
  override type Plus[That <: Natural] = Suc[N#Plus[That]]
  override type Mult2[M <: Natural] = N#Mult2[M]#Plus[M]
  override type Mult[M <: Natural] = M#Plus[N#Mult[M]]

  override type Previous = N
  override type Minus[M <: Natural] = M#_Minus[Suc[N]]
  override type _Minus[M <: Natural] = Previous#_Minus[M#Previous]

  // Methods
  override def plus[M <: Natural](m: M): Suc[N#Plus[M]] = Suc(n.plus(m))

  override def previous: N = n
  override def minus[M <: Natural](m: M): M#_Minus[Suc[N]] = m._minus(this)
  override def _minus[M <: Natural](m: M): N#_Minus[M#Previous] = previous._minus(m.previous)

  override def toInt: Int = 1 + n.toInt
}

/**
  * Helper types aliases for the natural numbers.
  *
  * Also, the [[Natural.sucN]] and [[Natural.zero]] leverage recursive implicit resolution to provide
  * an instance of a natural number (not the type, the actual number)
  *
  * @see The implicits at [[linkedlist.typedlist.TypedList]].
  * @example {{{
  *           val numberTen = implicitly[Nat10]
  *           //res = 10
  * }}}
  */
object Natural {

  implicit def sucN[N <: Natural](implicit prev: N): Suc[N] = Suc(prev)
  implicit val zero: Zero.type = Zero

  type Nat0 = Zero.type
  type Nat1 = Suc[Nat0]
  type Nat2 = Suc[Nat1]
  type Nat3 = Suc[Nat2]
  type Nat4 = Suc[Nat3]
  type Nat5 = Suc[Nat4]
  type Nat6 = Suc[Nat5]
  type Nat7 = Suc[Nat6]
  type Nat8 = Suc[Nat7]
  type Nat9 = Suc[Nat8]
  type Nat10 = Suc[Nat9]
  type Nat11 = Suc[Nat10]
  type Nat12 = Suc[Nat11]
  type Nat13 = Suc[Nat12]
  type Nat14 = Suc[Nat13]

  val nat0: Nat0 = Zero
  val nat1: Nat1 = Suc(nat0)
  val nat2: Nat2 = Suc(nat1)
  val nat3: Nat3 = Suc(nat2)
  val nat4: Nat4 = Suc(nat3)
  val nat5: Nat5 = Suc(nat4)
  val nat6: Nat6 = Suc(nat5)
  val nat7: Nat7 = Suc(nat6)
  val nat8: Nat8 = Suc(nat7)
  val nat9: Nat9 = Suc(nat8)
  val nat10: Nat10 = Suc(nat9)
  val nat11: Nat11 = Suc(nat10)
  val nat12: Nat12 = Suc(nat11)
  val nat13: Nat13 = Suc(nat12)
  val nat14: Nat14 = Suc(nat13)
}
