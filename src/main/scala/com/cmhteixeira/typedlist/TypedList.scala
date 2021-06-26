package com.cmhteixeira.typedlist

import com.cmhteixeira.typedlist.naturalnumbers.{LowerOrEqual, Natural, Suc, Zero}

/** A linked list with compile time size.
  *
  * @see package [[naturalnumbers]]
  * @example {{{
  * import com.cmhteixeira.naturalnumbers.Natural._
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
sealed trait TypedList[Size <: Natural, +Element] {
  protected def _head: Element

  /** Selects the first element of this list.
    *
    *  @return The first element of this list.
    */
  def head[PhantomType >: Size <: Suc[_ <: Natural]]: Element = _head

  protected def _tail: TypedList[Size#Previous, Element]

  /** The list without its first element.
    *
    *  @return The rest of the list without its first element..
    */
  def tail[PhantomType >: Size <: Suc[_ <: Natural]]: TypedList[Size#Previous, Element] = _tail

  /** Builds a new list by applying a function to all elements of this list.
    *
    * The new list has the same size as this list.
    *
    *  @param f The function to apply to each element.
    *  @tparam OtherType The element type of the returned $coll.
    *  @return A new list resulting from applying the given function `f`
    *          to each element of this list and collecting the results.
    */
  def map[OtherType](f: Element => OtherType): TypedList[Size, OtherType]

  /** Returns a new list formed from this list and list `that` by applying function [[f]] to the elements
    * of said lists at each position.
    *
    * @param that The list providing the second half of each result pair
    * @param f The function to be applied to each pair containing the element of this list and `that` list for each
    *          position.
    * @tparam OtherType The type of the elements of the `that` list.
    * @tparam C The return type of the function [[f]] that is applied to the pair of elements.
    * @return A new list formed by applying a function to the tuple of elements from both lists at a given position.
    */
  def zip[OtherType, C](that: TypedList[Size, OtherType], f: (Element, OtherType) => C): TypedList[Size, C]

  /** Returns a new list formed from this list and list `that` by applying function [[f]] to the elements
    * of said lists at each position.
    *
    * @param that The list providing the second half of each result pair
    * @param f The function to be applied to each pair containing the element of this list and `that` list for each
    *          position.
    * @tparam OtherType The type of the elements of the `that` list.
    * @tparam C The return type of the function [[f]] that is applied to the pair of elements.
    * @return A new list formed by applying a function to the tuple of elements from both lists at a given position.
    */
  def zip2[OtherType, C](that: TypedList[Size, OtherType], f: (Element, OtherType) => C): TypedList[Size, C]

  /** Returns a new list formed from this list and another list of the same size
    *  by combining corresponding elements in pairs.
    *
    * The length of the returned list is the same as both the this list and `that`
    *
    *  @param that The list providing the second half of each result pair
    *  @tparam OtherType The type of the second half of the returned pairs
    *  @return A new list containing pairs consisting of corresponding elements of this list and `that`.
    */
  def zip[OtherType](that: TypedList[Size, OtherType]): TypedList[Size, (Element, OtherType)] =
    zip(that, (a: Element, b: OtherType) => (a, b))

  /** Returns a new list containing the elements from the left hand operand followed by the elements from the
    *  right hand operand. The element type of the $coll is the most specific superclass encompassing
    *  the element types of the two operands.
    *
    *  @param that The list to append.
    *  @tparam OtherType The element type of the returned collection.
    *  @return A new list which contains all elements of this list followed by all elements of `that`.
    */
  def concat[OtherType >: Element, OtherSize <: Natural](
      that: TypedList[OtherSize, OtherType]
  ): TypedList[Size#Plus[OtherSize], OtherType]

  /** Builds a new list by applying a function to all elements of this list
    *  and using the elements of the resulting collections.
    *
    *
    *  @see [[flatMap2]]
    *  @param f The function to apply to each element.
    *  @tparam OtherType The element type of the returned collection.
    *  @tparam OtherSize The size of the lists returning from applying the function to each element of this list.
    *  @return A new list resulting from applying the given collection-valued function
    *          `f` to each element of this list and concatenating the results.
    */
  def flatMap[OtherType, OtherSize <: Natural](
      f: Element => TypedList[OtherSize, OtherType]
  ): TypedList[Size#Mult[OtherSize], OtherType]

  /** Builds a new list by applying a function to all elements of this list
    *  and using the elements of the resulting collections.
    *
    *
    *  @see [[flatMap]]
    *  @param f The function to apply to each element.
    *  @tparam OtherType The element type of the returned collection.
    *  @tparam OtherSize The size of the lists returning from applying the function to each element of this list.
    *  @return A new list resulting from applying the given collection-valued function
    *          `f` to each element of this list and concatenating the results.
    */
  def flatMap2[OtherType, OtherSize <: Natural](
      f: Element => TypedList[OtherSize, OtherType]
  ): TypedList[Size#Mult2[OtherSize], OtherType]

  private[typedlist] def _flatMap2Internal[OtherSize <: Natural, OtherType](
      f: Element => TypedList[OtherSize, OtherType]
  ): TypedList[Size#Mult2[OtherSize], OtherType]

  /** A copy of this list with an element appended.
    *
    * @param elem The appended element
    * @tparam A1 The element type of the returned list.
    * @return A new list consisting of all elements of this list followed by `elem`.
    */
  def :+[A1 >: Element](elem: A1): TypedList[Suc[Size], A1]

  /** Adds an element at the beginning of this list.
    *
    *  @param elem The element to prepend.
    *  @return A list which contains `elem` as first element and which continues with this list.
    */
  def ::[A1 >: Element](elem: A1): TypedList[Suc[Size], A1]

  /** Returns new list with elements in reversed order.
    *
    *  @return A new list with all elements of this list in reversed order.
    */
  def reverse: TypedList[Size, Element]

  /** Splits the list into a prefix/suffix pair at a given position.
    *
    *  @tparam At Position at which to split.
    *  @return A pair of lists consisting of the first `At` elements of this list, and the other elements.
    */
  def split[At <: Suc[_ <: Natural]](
      implicit guaranteeIndexWithinBounds: At LowerOrEqual Size#Previous,
      at: At,
      before: TypedList[At, Natural],
      after: TypedList[Size#Minus[At], Natural]
  ): (TypedList[At, Element], TypedList[Size#Minus[At], Element])

  /** Returns the element of the list at a given position.
    *
    *  @tparam Index Position of element to be returned
    *  @return The element to be returned
    */
  def get[Index <: Suc[_ <: Natural]](implicit n: Index, guaranteeIndexWithinBounds: Index LowerOrEqual Size): Element =
    _get(n)

  private[typedlist] def _get(goalIndex: Natural): Element

  override final def toString: String = obtainElementAsString.fold("TypedList()")("TypedList(" + _ + ")")

  def obtainElementAsString: Option[String]

  /** Tests whether this list contains a given value as an element.
    *
    *  @param elem  The element to test.
    *  @return `true` if this list has an element that is equal (as determined by `==`) to `elem`, `false` otherwise.
    */
  def contains[A >: Element](elem: A): Boolean

  /** Counts the number of elements in the list which satisfy a predicate.
    *
    *  @param predicate The predicate used to test elements.
    *  @return The number of elements satisfying the predicate `predicate`
    */
  def count(predicate: Element => Boolean): Int

  /** Transform this typed list into a standard library list.
    *
    * @return A a new __normal__ list with the same elements as this list.
    */
  def toList: List[Element]

  /** The size of this list.
    *
    *  @return The number of elements in this list.
    */
  def size: Int = count(_ => true)

  /** Finds the first element of the list for which the given partial
    * function is defined, and applies the partial function to it.
    *
    *  @param p The partial function
    *  @return An option value containing `p` applied to the first value for which it is defined, or [[None]] if none exists.
    */
  def collectFirst[B](p: PartialFunction[Element, B]): Option[B]

  /** Tests whether a predicate holds for all elements of this list.
    *
    *  @param p The predicate used to test elements.
    *  @return `true` if this this is empty or the given predicate `p` holds for all
    *          elements of this this, otherwise `false`.
    */
  def forall(p: Element => Boolean): Boolean
}

case object TypedNil extends TypedList[Zero.type, Nothing] {

  override protected def _head: Nothing =
    throw new Exception("This exception will never be thrown since it is 'protected' by a phantom type.")

  override protected def _tail: TypedList[Zero.type, Nothing] =
    throw new Exception("This exception will never be thrown since it is 'protected' by a phantom type.")

  override def map[OtherType](f: Nothing => OtherType): TypedList[Zero.type, OtherType] = this

  override def zip[OtherType, C](
      that: TypedList[Zero.type, OtherType],
      f: (Nothing, OtherType) => C
  ): TypedList[Zero.type, C] = this

  override def zip2[OtherType, C](
      that: TypedList[Zero.type, OtherType],
      f: (Nothing, OtherType) => C
  ): TypedList[Zero.type, C] = this

  override def concat[OtherType >: Nothing, OtherSize <: Natural](
      that: TypedList[OtherSize, OtherType]
  ): TypedList[OtherSize, OtherType] = that

  override def flatMap[OtherType, OtherSize <: Natural](
      f: Nothing => TypedList[OtherSize, OtherType]
  ): TypedList[Zero.type, OtherType] = this

  override def flatMap2[OtherType, OtherSize <: Natural](
      f: Nothing => TypedList[OtherSize, OtherType]
  ): TypedList[Zero.type, OtherType] = this

  override private[typedlist] def _flatMap2Internal[OtherSize <: Natural, OtherType](
      f: Nothing => TypedList[OtherSize, OtherType]
  ): TypedList[Zero.type, OtherType] = this

  override def :+[A1 >: Nothing](elem: A1): TypedList[Suc[Zero.type], A1] = TypedCons(elem, TypedNil)

  override def ::[A1 >: Nothing](elem: A1): TypedList[Suc[Zero.type], A1] = TypedCons(elem, TypedNil)

  override def reverse: TypedList[Zero.type, Nothing] = this

  override def split[At <: Suc[_ <: Natural]](
      implicit guaranteeIndexWithinBounds: LowerOrEqual[At, Zero.type],
      at: At,
      before: TypedList[At, Natural],
      after: TypedList[Zero.type, Natural]
  ): (TypedList[At, Nothing], TypedList[Zero.type, Nothing]) =
    throw new Exception("This exception will never be thrown since it is 'protected' by type bounds and implicits.")

  override private[typedlist] def _get(goalIndex: Natural): Nothing =
    throw new Exception(
      "This exception will never be thrown for any manipulations the user might do. Also, it is not on the API so the user cannot call it directly."
    )

  override def obtainElementAsString: Option[String] = None

  override def count(predicate: Nothing => Boolean): Int = 0

  override def contains[A >: Nothing](elem: A): Boolean = false

  override def toList: List[Nothing] = Nil

  override def collectFirst[B](p: PartialFunction[Nothing, B]): Option[B] = None

  override def forall(p: Nothing => Boolean): Boolean = true
}

case class TypedCons[Size <: Natural, Element](
    override protected val _head: Element,
    override protected val _tail: TypedList[Size, Element]
) extends TypedList[Suc[Size], Element] {

  override def map[OtherType](f: Element => OtherType): TypedList[Suc[Size], OtherType] =
    TypedCons(f(head), tail.map(f))

  override def zip[OtherType, C](
      that: TypedList[Suc[Size], OtherType],
      f: (Element, OtherType) => C
  ): TypedList[Suc[Size], C] = that match {
    case TypedCons(thatHead, thatTail) => TypedCons(f(head, thatHead), tail.zip(thatTail, f))
  }

  override def zip2[OtherType, C](
      that: TypedList[Suc[Size], OtherType],
      f: (Element, OtherType) => C
  ): TypedList[Suc[Size], C] =
    TypedCons(f(head, that.head), tail.zip2(that.tail, f))

  override def concat[OtherType >: Element, OtherSize <: Natural](
      that: TypedList[OtherSize, OtherType]
  ): TypedList[Suc[Size#Plus[OtherSize]], OtherType] =
    TypedCons(head, tail.concat(that))

  override def flatMap[OtherType, OtherSize <: Natural](
      f: Element => TypedList[OtherSize, OtherType]
  ): TypedList[OtherSize#Plus[Size#Mult[OtherSize]], OtherType] =
    f(head) concat tail.flatMap(f)

  override def flatMap2[OtherType, OtherSize <: Natural](
      f: Element => TypedList[OtherSize, OtherType]
  ): TypedList[Size#Mult2[OtherSize]#Plus[OtherSize], OtherType] =
    _flatMap2Internal(f).reverse

  override private[typedlist] def _flatMap2Internal[OtherSize <: Natural, OtherType](
      f: Element => TypedList[OtherSize, OtherType]
  ): TypedList[Size#Mult2[OtherSize]#Plus[OtherSize], OtherType] =
    tail._flatMap2Internal(f) concat f(head)

  override def reverse: TypedList[Suc[Size], Element] = tail.reverse :+ head

  override def :+[A1 >: Element](elem: A1): TypedCons[Suc[Size], A1] = TypedCons(head, tail.:+(elem))

  override def ::[A1 >: Element](elem: A1): TypedList[Suc[Suc[Size]], A1] = TypedCons(elem, this)

  override def split[At <: Suc[_ <: Natural]](
      implicit guaranteeIndexWithinBounds: At LowerOrEqual Size,
      at: At,
      before: TypedList[At, Natural],
      after: TypedList[Suc[Size]#Minus[At], Natural]
  ): (TypedList[At, Element], TypedList[Suc[Size]#Minus[At], Element]) =
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

  override def toList: List[Element] = head +: tail.toList

  override def collectFirst[B](p: PartialFunction[Element, B]): Option[B] =
    if (p.isDefinedAt(head)) Some(p(head)) else tail.collectFirst(p)

  override def forall(p: Element => Boolean): Boolean = p(head) && tail.forall(p)
}

/**
  * The [[TypedList.typedListOfNats]] [[TypedList.emptyList]] implicit functions, leverage iterative implicit resolution to
  * automatically construct a typed list of naturals of the specified size.
  *
  * @note The typed list will have a size one greater than the value specified in the type parameter.
  * @example {{{
  *          val oneToEleven = implicitly[[TypedList[Nat10, Natural]]]
  *          //res = TypedList(1, 2, 3, 4, 5, 6, 7, 8, 9, 11)
  * }}}
  */
object TypedList extends support4cats.Implicits {

  implicit def typedListOfNats[N <: Natural](
      implicit previousNatTypedList: TypedList[N, Natural],
      thisNat: Suc[N]
  ): TypedList[Suc[N], Natural] =
    previousNatTypedList :+ thisNat

  implicit def emptyList[A]: TypedList[Zero.type, A] = TypedNil
}
