package com.cmhteixeira.typedlist

import cats.data.NonEmptyList
import cats.{Applicative, Eval}
import com.cmhteixeira.typedlist.naturalnumbers.Natural.Nat1
import com.cmhteixeira.typedlist.naturalnumbers.{LowerOrEqual, Natural, Suc, Zero}

import scala.util.control.TailCalls.{TailRec, done, tailcall}

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
  //Method is nilary instead of nullary to overcome Scala 2.x bug @ https://github.com/scala/bug/issues/12413.
  def head[PhantomType >: Size <: Suc[_ <: Natural]](): Element = _head

  protected def _tail: TypedList[Size#Previous, Element]

  /** The list without its first element.
    *
    *  @return The rest of the list without its first element..
    */
  //Method is nilary instead of nullary to overcome Scala 2.x bug @ https://github.com/scala/bug/issues/12413.
  def tail[PhantomType >: Size <: Suc[_ <: Natural]](): TypedList[Size#Previous, Element] = _tail

  /** Builds a new list by applying a function to all elements of this list.
    *
    * The new list has the same size as this list.
    *
    *  @param f The function to apply to each element.
    *  @tparam OtherType The element type of the returned $coll.
    *  @return A new list resulting from applying the given function `f`
    *          to each element of this list and collecting the results.
    */
  def map[OtherType](f: Element => OtherType): TypedList[Size, OtherType] =
    traverseHelper[Lambda[Y => Y], OtherType](a => done(f(a))).result

  /** Returns a new list formed from this list and list `that` by applying function `f` to the elements
    * of said lists at each position.
    *
    * @param that The list providing the second half of each result pair
    * @param f The function to be applied to each pair containing the element of this list and `that` list for each
    *          position.
    * @tparam OtherType The type of the elements of the `that` list.
    * @tparam C The return type of the function `f` that is applied to the pair of elements.
    * @return A new list formed by applying a function to the tuple of elements from both lists at a given position.
    */
  def zip[OtherType, C](that: TypedList[Size, OtherType], f: (Element, OtherType) => C): TypedList[Size, C]

  /** Returns a new list formed from this list and list `that` by applying function `f` to the elements
    * of said lists at each position.
    *
    * @param that The list providing the second half of each result pair
    * @param f The function to be applied to each pair containing the element of this list and `that` list for each
    *          position.
    * @tparam OtherType The type of the elements of the `that` list.
    * @tparam C The return type of the function `f` that is applied to the pair of elements.
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
    *  right hand operand. The element type of the list is the most specific superclass encompassing
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
  // The type cast should be appropriate here
  final def reverse: TypedList[Size, Element] =
    foldLeft[TypedList[_ <: Natural, Element]](TypedNil)((acc, elem) => TypedCons(elem, acc))
      .asInstanceOf[TypedList[Size, Element]]

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

  override final def toString: String =
    "TypedList(" + foldLeft("")((acc, elem) => s"$acc $elem,").dropRight(1).drop(1) + ")"

  /** Tests whether this list contains a given value as an element.
    *
    *  @param elem  The element to test.
    *  @return `true` if this list has an element that is equal (as determined by `==`) to `elem`, `false` otherwise.
    */
  final def contains[A >: Element](elem: A): Boolean =
    foldLeft(false)((acc, newElem) => acc || elem == newElem)

  /** Counts the number of elements in the list which satisfy a predicate.
    *
    *  @param predicate The predicate used to test elements.
    *  @return The number of elements satisfying the predicate `predicate`
    */
  final def count(predicate: Element => Boolean): Int =
    foldLeft(0)((acc, elem) => if (predicate(elem)) acc + 1 else acc)

  /** Transform this typed list into a standard library list.
    *
    * @return A new __normal__ list with the same elements as this list.
    */
  final def toList: List[Element] = foldRight(List.empty[Element])((elem, acc) => elem :: acc)

  /** The size of this list.
    *
    *  @return The number of elements in this list.
    */
  final def size: Int = count(_ => true)

  /** Finds the first element of the list for which the given partial
    * function is defined, and applies the partial function to it.
    *
    *  @param p The partial function
    *  @return An option value containing `p` applied to the first value for which it is defined, or [[None]] if none exists.
    */
  final def collectFirst[B](p: PartialFunction[Element, B]): Option[B] =
    foldLeft[Option[B]](None)((acc, elem) => if (acc.isDefined) acc else p.lift(elem))

  /** Tests whether a predicate holds for all elements of this list.
    *
    *  @param p The predicate used to test elements.
    *  @return `true` if this this is empty or the given predicate `p` holds for all
    *          elements of this this, otherwise `false`.
    */
  final def forall(p: Element => Boolean): Boolean =
    foldLeft(true)((acc, elem) => acc && p(elem))

  /** A copy of this list with one single replaced element.
    *
    *  @param elem The replacing element
    *  @tparam B The element type of the returned list.
    *  @tparam Index The index of the replacement.
    *  @return A new list which is a copy of this list with the element at position `Index` replaced by `elem`.
    */
  def updated[Index <: Natural, B >: Element](elem: B)(
      implicit guaranteeIndexWithinBounds: Index LowerOrEqual Size,
      index: Index
  ): TypedList[Size, B]

  private[typedlist] def updatedHelper[B >: Element](
      elem: B,
      currentIndex: Int,
      targetIndex: Int
  ): TypedList[Size, B]

  /** Optionally selects the first element.
    *
    *  @return The first element of this list if it is nonempty, `None` if it is empty.
    */
  def headOption: Option[Element]

  /** Optionally selects the last element.
    *
    *  @return The last element of this list if it is nonempty, `None` if it is empty.
    */
  final def lastOption: Option[Element] =
    foldLeft[Option[Element]](None)((_, a) => Some(a))

  /** Applies a binary operator to a start value and all elements of this list,
    *  going left to right.
    *
    *  @param b The start value.
    *  @param f The binary operator.
    *  @tparam B The result type of the binary operator.
    *  @return The result of inserting `f` between consecutive elements of this list,
    *           going left to right with the start value `b` on the left:
    *           `f(...f(b, x,,1,,), x,,2,,, ..., x,,n,,)` where `x,,1,,, ..., x,,n,,`
    *            are the elements of this $coll.
    *           Returns `b` if the list is empty.
    */
  final def foldLeft[B](b: B)(f: (B, Element) => B): B = TypedList.foldLeft(this, b)(f)

  /** Applies a binary operator to all elements of this list and a start value,
    *  going right to left.
    *
    *  @param b The start value.
    *  @param f The binary operator.
    *  @tparam B The result type of the binary operator.
    *  @return The result of inserting `f` between consecutive elements of this list,
    *           going right to left with the start value `z` on the right:
    *           `f(x,,1,,, f(x,,2,,, ... f(x,,n,,, z)...))` where `x,,1,,, ..., x,,n,,`
    *           are the elements of this $coll.
    *           Returns `b` if this list is empty.
    */
  final def foldRight[B](b: B)(f: (Element, B) => B): B = {
    implicit def myApplicative[P]: Applicative[Lambda[A => Function[TailRec[P], TailRec[P]]]] =
      new Applicative[Lambda[A => Function[TailRec[P], TailRec[P]]]] {
        override def pure[K](x: K): Function[TailRec[P], TailRec[P]] = identity

        override def ap[K, L](
            ff: Function[TailRec[P], TailRec[P]]
        )(fa: Function[TailRec[P], TailRec[P]]): Function[TailRec[P], TailRec[P]] =
          b => tailcall(ff(fa(b)))
      }

    traverseHelper[Lambda[Y => Function[TailRec[B], TailRec[B]]], B](a =>
      done((bRec: TailRec[B]) => bRec.map(b => f(a, b)))
    ).flatMap(f => f(done(b))).result
  }

  private[typedlist] def traverseHelper[G[_]: Applicative, B](
      f: Element => TailRec[G[B]]
  ): TailRec[G[TypedList[Size, B]]]

  private[typedlist] def traverseHelperCats[G[_]: Applicative, B](f: Element => Eval[G[B]]): Eval[G[TypedList[Size, B]]]

}

case object TypedNil extends TypedList[Zero.type, Nothing] {

  override protected def _head: Nothing =
    throw new Exception("This exception will never be thrown since it is 'protected' by a phantom type.")

  override protected def _tail: TypedList[Zero.type, Nothing] =
    throw new Exception("This exception will never be thrown since it is 'protected' by a phantom type.")

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

  override def updated[Index <: Natural, B >: Nothing](elem: B)(
      implicit guaranteeIndexWithinBounds: LowerOrEqual[Index, Zero.type],
      index: Index
  ): TypedList[Zero.type, B] = this

  override private[typedlist] def updatedHelper[B >: Nothing](
      elem: B,
      currentIndex: Int,
      targetIndex: Int
  ): TypedList[Zero.type, B] = this

  override def headOption: Option[Nothing] = None

  override private[typedlist] def traverseHelper[G[_]: Applicative, B](
      f: Nothing => TailRec[G[B]]
  ): TailRec[G[TypedList[Zero.type, B]]] =
    done(Applicative[G].pure(this))

  override private[typedlist] def traverseHelperCats[G[_]: Applicative, B](
      f: Nothing => Eval[G[B]]
  ): Eval[G[TypedList[Zero.type, B]]] =
    Eval.now(Applicative[G].pure(this))
}

case class TypedCons[Size <: Natural, Element](
    override protected val _head: Element,
    override protected val _tail: TypedList[Size, Element]
) extends TypedList[Suc[Size], Element] {

  override def zip[OtherType, C](
      that: TypedList[Suc[Size], OtherType],
      f: (Element, OtherType) => C
  ): TypedList[Suc[Size], C] = that match {
    case TypedCons(thatHead, thatTail) => TypedCons(f(head(), thatHead), tail().zip(thatTail, f))
  }

  override def zip2[OtherType, C](
      that: TypedList[Suc[Size], OtherType],
      f: (Element, OtherType) => C
  ): TypedList[Suc[Size], C] =
    TypedCons(f(head(), that.head()), tail().zip2(that.tail(), f))

  override def concat[OtherType >: Element, OtherSize <: Natural](
      that: TypedList[OtherSize, OtherType]
  ): TypedList[Suc[Size#Plus[OtherSize]], OtherType] =
    TypedCons(head(), tail().concat(that))

  override def flatMap[OtherType, OtherSize <: Natural](
      f: Element => TypedList[OtherSize, OtherType]
  ): TypedList[OtherSize#Plus[Size#Mult[OtherSize]], OtherType] =
    f(head()) concat tail().flatMap(f)

  override def flatMap2[OtherType, OtherSize <: Natural](
      f: Element => TypedList[OtherSize, OtherType]
  ): TypedList[Size#Mult2[OtherSize]#Plus[OtherSize], OtherType] =
    _flatMap2Internal(f).reverse

  override private[typedlist] def _flatMap2Internal[OtherSize <: Natural, OtherType](
      f: Element => TypedList[OtherSize, OtherType]
  ): TypedList[Size#Mult2[OtherSize]#Plus[OtherSize], OtherType] =
    tail()._flatMap2Internal(f) concat f(head())

  override def :+[A1 >: Element](elem: A1): TypedCons[Suc[Size], A1] = TypedCons(head(), tail().:+(elem))

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
      head()
    } else {
      tail()._get(goalIndex.previous)
    }
  }

  override def updated[Index <: Natural, B >: Element](elem: B)(
      implicit guaranteeIndexWithinBounds: LowerOrEqual[Index, Suc[Size]],
      index: Index
  ): TypedList[Suc[Size], B] = updatedHelper(elem, 1, index.toInt)

  override private[typedlist] def updatedHelper[B >: Element](
      elem: B,
      currentIndex: Int,
      targetIndex: Int
  ): TypedList[Suc[Size], B] =
    if (currentIndex < targetIndex) TypedCons(head(), _tail.updatedHelper(elem, currentIndex + 1, targetIndex))
    else if (currentIndex == targetIndex) TypedCons(elem, tail())
    else this

  override def headOption: Option[Element] = Some(_head)

  override private[typedlist] def traverseHelper[G[_]: Applicative, B](
      f: Element => TailRec[G[B]]
  ): TailRec[G[TypedList[Suc[Size], B]]] =
    for {
      current <- f(_head)
      rest <- tailcall(_tail.traverseHelper(f))
    } yield Applicative[G].map2(current, rest)((i, xs) => i :: xs)

  override private[typedlist] def traverseHelperCats[G[_]: Applicative, B](
      f: Element => Eval[G[B]]
  ): Eval[G[TypedList[Suc[Size], B]]] = {
    for {
      current <- f(_head)
      rest <- Eval.defer(_tail.traverseHelperCats(f))
    } yield Applicative[G].map2(current, rest)((i, xs) => i :: xs)
  }
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

  // This is defined here instead of a normal method because of a scala-compiler bug https://github.com/scala/bug/issues/9394
  private def foldLeft[Size <: Natural, Element, B](fa: TypedList[Size, Element], b: B)(f: (B, Element) => B): B =
    fa match {
      case TypedNil => b
      case TypedCons(_head, _tail) => foldLeft(_tail, f(b, _head))(f)
    }

  def empty[A]: TypedList[Zero.type, A] = TypedNil

  def fromList[Size <: Suc[_ <: Natural], A](list: NonEmptyList[A])(
      implicit t: TypedList[Size, Natural]
  ): Option[TypedList[Size, A]] = fromList(list.toList)

  def fromList[Size <: Natural, A](
      list: List[A]
  )(implicit t: TypedList[Size, Natural]): Option[TypedList[Size, A]] = {
    (list, t) match {
      case (Nil, TypedNil) => Some(TypedNil)
      case (Nil, TypedCons(_, _)) => None
      case (head :: tail, TypedNil) => None
      case (head :: tail, TypedCons(_, _tail)) =>
        fromList[Size#Minus[Nat1], A](tail)(_tail)
          .map(o => head :: o)
    }
  }

  implicit def typedListOfNats[N <: Natural](
      implicit previousNatTypedList: TypedList[N, Natural],
      thisNat: Suc[N]
  ): TypedList[Suc[N], Natural] =
    previousNatTypedList :+ thisNat

  implicit def emptyList[A]: TypedList[Zero.type, A] = TypedNil
}
