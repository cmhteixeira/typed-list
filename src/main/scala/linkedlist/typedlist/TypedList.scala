package linkedlist.typedlist

import linkedlist.naturalnumbers.{LowerOrEqual, Natural, Suc, Zero}

/** A linked list with compile time size.
  *
  * @see package [[linkedlist.naturalnumbers]]
  * @example {{{
  * import linkedlist.naturalnumbers.Natural._
  *
  * val aListOfSizeEight = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: TypedNil
  *
  * // Compile error when accessing element outside bounds (as the bounds are known at compile time)
  * aListOfSizeEight.get[Nat10]
  *
  * // Compile error when accessing head/tail of empty list
  * val aListOfSizeOne = "Foo" :: TypedNil
  * val anEmptyList = aListOfSizeOne.tail
  *
  * // You can get an element at a specific index
  * val elemAtIndex = aListOfSizeEight.get[Nat3]
  *
  * // You can split into two lists maintaining typed size on both.
  * val (ofSize2, ofSize6) = aListOfSizeEight.split[Nat2]
  *
  * // You can map over the list
  * val stringList = aListOfSizeEight.map(i => s"cmhteixeira-$i")
  *
  * // You can concatenate two lists and the resulting list has correct typed size
  * val firstList = "Foo" :: "Bar" :: "Baz" :: TypedNil
  * val secondList = "Qux" :: "Quux" :: TypedNil
  * val concatenatedList = firstList concat secondList
  *
  * // You can flatmap the list, and the resulting list will have the correct size !! -> natural multiplication
  * val someList = "Foo" :: "Bar" :: "Baz" :: TypedNil
  * val result = someList.flatMap(i => s"$i-1" :: s"$i-2" :: TypedNil)
  * }}}
  * @tparam Element Refers to the contents of the list, the same away as the `A` on the
  *                 standard library's List[A]. It is covariant for the same reason
  *                 as the List is.
  * @tparam Size Natural number describing the size of the list.
  */
sealed trait TypedList[+Element, Size <: Natural] {
  protected def _head: Element
  def head[PhantomType >: Size <: Suc[_ <: Natural]]: Element = _head

  protected def _tail: TypedList[Element, Size#Previous]
  def tail[PhantomType >: Size <: Suc[_ <: Natural]]: TypedList[Element, Size#Previous] = _tail

  def map[OtherType](f: Element => OtherType): TypedList[OtherType, Size]

  def zip[OtherType, C](that: TypedList[OtherType, Size], f: (Element, OtherType) => C): TypedList[C, Size]
  def zip2[OtherType, C](that: TypedList[OtherType, Size], f: (Element, OtherType) => C): TypedList[C, Size]

  def concat[OtherType >: Element, OtherSize <: Natural](
      that: TypedList[OtherType, OtherSize]
  ): TypedList[OtherType, Size#Plus[OtherSize]]

  def flatMap[OtherType, OtherSize <: Natural](
      f: Element => TypedList[OtherType, OtherSize]
  ): TypedList[OtherType, Size#Mult[OtherSize]]

  def flatMap2[OtherType, OtherSize <: Natural](
      f: Element => TypedList[OtherType, OtherSize]
  ): TypedList[OtherType, Size#Mult2[OtherSize]]

  private[typedlist] def _flatMap2Internal[OtherType, OtherSize <: Natural](
      f: Element => TypedList[OtherType, OtherSize]
  ): TypedList[OtherType, Size#Mult2[OtherSize]]

  def :+[A1 >: Element](elem: A1): TypedList[A1, Suc[Size]]

  def ::[A1 >: Element](elem: A1): TypedList[A1, Suc[Size]]

  def reverse: TypedList[Element, Size]

  def split[At <: Suc[_ <: Natural]](
      implicit guaranteeIndexWithinBounds: At LowerOrEqual Size#Previous,
      at: At,
      before: TypedList[Natural, At],
      after: TypedList[Natural, Size#Minus[At]]
  ): (TypedList[Element, At], TypedList[Element, Size#Minus[At]])

  def get[Index <: Suc[_ <: Natural]](implicit n: Index, guaranteeIndexWithinBounds: Index LowerOrEqual Size): Element =
    _get(n)

  private[typedlist] def _get(goalIndex: Natural): Element

  override final def toString: String = obtainElementAsString.fold("TypedList()")("TypedList(" + _ + ")")
  def obtainElementAsString: Option[String]

  def contains[A >: Element](elem: A): Boolean

  def count(predicate: Element => Boolean): Int
}

case object TypedNil extends TypedList[Nothing, Zero.type] {

  override protected def _head: Nothing =
    throw new Exception("This exception will never be thrown since it is 'protected' by a phantom type.")

  override protected def _tail: TypedList[Nothing, Zero.type] =
    throw new Exception("This exception will never be thrown since it is 'protected' by a phantom type.")

  override def map[OtherType](f: Nothing => OtherType): TypedList[OtherType, Zero.type] = this

  override def zip[OtherType, C](
      that: TypedList[OtherType, Zero.type],
      f: (Nothing, OtherType) => C
  ): TypedList[C, Zero.type] = this

  override def zip2[OtherType, C](
      that: TypedList[OtherType, Zero.type],
      f: (Nothing, OtherType) => C
  ): TypedList[C, Zero.type] = this

  override def concat[OtherType >: Nothing, OtherSize <: Natural](
      that: TypedList[OtherType, OtherSize]
  ): TypedList[OtherType, OtherSize] = that

  override def flatMap[OtherType, OtherSize <: Natural](
      f: Nothing => TypedList[OtherType, OtherSize]
  ): TypedList[OtherType, Zero.type] = this

  override def flatMap2[OtherType, OtherSize <: Natural](
      f: Nothing => TypedList[OtherType, OtherSize]
  ): TypedList[OtherType, Zero.type] = this

  override private[typedlist] def _flatMap2Internal[OtherType, OtherSize <: Natural](
      f: Nothing => TypedList[OtherType, OtherSize]
  ): TypedList[OtherType, Zero.type] = this

  override def :+[A1 >: Nothing](elem: A1): TypedList[A1, Suc[Zero.type]] = TypedCons(elem, TypedNil)

  override def ::[A1 >: Nothing](elem: A1): TypedList[A1, Suc[Zero.type]] = TypedCons(elem, TypedNil)

  override def reverse: TypedList[Nothing, Zero.type] = this

  override def split[At <: Suc[_ <: Natural]](
      implicit guaranteeIndexWithinBounds: LowerOrEqual[At, Zero.type],
      at: At,
      before: TypedList[Natural, At],
      after: TypedList[Natural, Zero.type]
  ): (TypedList[Nothing, At], TypedList[Nothing, Zero.type]) =
    throw new Exception("This exception will never be thrown since it is 'protected' by type bounds and implicits.")

  override private[typedlist] def _get(goalIndex: Natural): Nothing =
    throw new Exception(
      "This exception will never be thrown for any manipulations the user might do. Also, it is not on the API so the user cannot call it directly."
    )

  override def obtainElementAsString: Option[String] = None

  override def count(predicate: Nothing => Boolean): Int = 0

  override def contains[A >: Nothing](elem: A): Boolean = false
}

case class TypedCons[Element, Size <: Natural](
    override protected val _head: Element,
    override protected val _tail: TypedList[Element, Size]
) extends TypedList[Element, Suc[Size]] {

  override def map[OtherType](f: Element => OtherType): TypedList[OtherType, Suc[Size]] =
    TypedCons(f(head), tail.map(f))

  override def zip[OtherType, C](
      that: TypedList[OtherType, Suc[Size]],
      f: (Element, OtherType) => C
  ): TypedList[C, Suc[Size]] = that match {
    case TypedCons(thatHead, thatTail) => TypedCons(f(head, thatHead), tail.zip(thatTail, f))
  }

  override def zip2[OtherType, C](
      that: TypedList[OtherType, Suc[Size]],
      f: (Element, OtherType) => C
  ): TypedList[C, Suc[Size]] =
    TypedCons(f(head, that.head), tail.zip2(that.tail, f))

  override def concat[OtherType >: Element, OtherSize <: Natural](
      that: TypedList[OtherType, OtherSize]
  ): TypedList[OtherType, Suc[Size#Plus[OtherSize]]] =
    TypedCons(head, tail.concat(that))

  override def flatMap[OtherType, OtherSize <: Natural](
      f: Element => TypedList[OtherType, OtherSize]
  ): TypedList[OtherType, OtherSize#Plus[Size#Mult[OtherSize]]] =
    f(head) concat tail.flatMap(f)

  override def flatMap2[OtherType, OtherSize <: Natural](
      f: Element => TypedList[OtherType, OtherSize]
  ): TypedList[OtherType, Size#Mult2[OtherSize]#Plus[OtherSize]] =
    _flatMap2Internal(f).reverse

  override private[typedlist] def _flatMap2Internal[OtherType, OtherSize <: Natural](
      f: Element => TypedList[OtherType, OtherSize]
  ): TypedList[OtherType, Size#Mult2[OtherSize]#Plus[OtherSize]] =
    tail._flatMap2Internal(f) concat f(head)

  override def reverse: TypedList[Element, Suc[Size]] = tail.reverse :+ head

  override def :+[A1 >: Element](elem: A1): TypedCons[A1, Suc[Size]] = TypedCons(head, tail.:+(elem))

  override def ::[A1 >: Element](elem: A1): TypedList[A1, Suc[Suc[Size]]] = TypedCons(elem, this)

  override def split[At <: Suc[_ <: Natural]](
      implicit guaranteeIndexWithinBounds: At LowerOrEqual Size,
      at: At,
      before: TypedList[Natural, At],
      after: TypedList[Natural, Suc[Size]#Minus[At]]
  ): (TypedList[Element, At], TypedList[Element, Suc[Size]#Minus[At]]) =
    (before.map(_get), after.map(i => _get(i.plus(at))))

  override private[typedlist] def _get(goalIndex: Natural): Element = {
    if (goalIndex == Suc(Zero)) {
      head
    } else {
      tail._get(goalIndex.previous)
    }
  }

  override def obtainElementAsString: Option[String] = Some(tail.obtainElementAsString.fold(s"$head")(s"$head, " + _))

  override def count(predicate: Element => Boolean): Int =
    if (predicate(head)) 1 + tail.count(predicate) else tail.count(predicate)

  override def contains[A >: Element](elem: A): Boolean =
    head == elem || tail.contains(elem)
}

/**
  * The [[TypedList.typedListOfNats]] [[TypedList.emptyList]] implicit functions, leverage iterative implicit resolution to
  * automatically construct a typed list of naturals of the specified size.
  *
  * @note The typed list will have a size one greater than the value specified in the type parameter.
  * @example {{{
  *          val oneToEleven = implicitly[[TypedList[Natural, Nat10]]]
  *          //res = TypedList(1, 2, 3, 4, 5, 6, 7, 8, 9, 11)
  * }}}
  */
object TypedList {

  implicit def typedListOfNats[N <: Natural](
      implicit previousNatTypedList: TypedList[Natural, N],
      thisNat: Suc[N]
  ): TypedList[Natural, Suc[N]] =
    previousNatTypedList :+ thisNat

  implicit def emptyList[A]: TypedList[A, Zero.type] = TypedNil
}
