package linkedlist.typedlist

import linkedlist.naturalnumbers.{LowerOrEqual, Natural, Suc, Zero}

sealed trait TypedList[+Element, Size <: Natural]{
  protected def _head: Element
  def head[PhantomType >: Size <: Suc[_ <: Natural]]: Element = _head

  protected def _tail: TypedList[Element, Size#Previous]
  def tail[PhantomType >:  Size <: Suc[_ <: Natural]]: TypedList[Element, Size#Previous] = _tail

  def map[OtherType](f: Element => OtherType): TypedList[OtherType, Size]

  def zip[OtherType, C](that: TypedList[OtherType, Size], f: (Element, OtherType) => C): TypedList[C, Size]
  def zip2[OtherType, C](that: TypedList[OtherType, Size], f: (Element, OtherType) => C): TypedList[C, Size]

  def concat[OtherType >: Element, OtherSize <: Natural](that: TypedList[OtherType, OtherSize]): TypedList[OtherType, Size#Plus[OtherSize]]

  def flatMap[OtherType, OtherSize <: Natural](f: Element => TypedList[OtherType, OtherSize]): TypedList[OtherType, Size#Mult[OtherSize]]

  def flatMap2[OtherType, OtherSize <: Natural](f: Element => TypedList[OtherType, OtherSize]): TypedList[OtherType, Size#Mult2[OtherSize]]
  private [typedlist] def _flatMap2Internal[OtherType, OtherSize <: Natural](f: Element => TypedList[OtherType, OtherSize]): TypedList[OtherType, Size#Mult2[OtherSize]]

  def :+[A1 >: Element](elem: A1): TypedList[A1, Suc[Size]]

  def ::[A1 >: Element](elem: A1): TypedList[A1, Suc[Size]]

  def reverse: TypedList[Element, Size]

  def split[At <: Suc[_ <: Natural]](
    implicit guaranteeIndexWithinBounds: At LowerOrEqual Size#Previous,
    at: At,
    before: TypedList[Natural, At],
    after: TypedList[Natural, Size#Minus[At]]): (TypedList[Element, At], TypedList[Element, Size#Minus[At]])

  def get[Index <: Suc[_ <: Natural]](
    implicit n: Index,
    guaranteeIndexWithinBounds: Index LowerOrEqual Size): Element = _get(n)

  private [typedlist] def _get(goalIndex: Natural): Element

  override final def toString: String = obtainElementAsString.fold("TypedList()")("TypedList(" + _ + ")")
  def obtainElementAsString: Option[String]

  def contains[A >: Element](elem: A): Boolean

  def count(predicate: Element => Boolean): Int
}



case object TypedNil extends TypedList[Nothing, Zero.type]{
  override protected def _head: Nothing = throw new Exception("Boom!")

  override protected def _tail: TypedList[Nothing, Zero.type] = throw new Exception("Boom!")

  override def map[OtherType](f: Nothing => OtherType): TypedList[OtherType, Zero.type] = this

  override def zip[OtherType, C](that: TypedList[OtherType, Zero.type], f: (Nothing, OtherType) => C): TypedList[C, Zero.type] = this
  override def zip2[OtherType, C](that: TypedList[OtherType, Zero.type], f: (Nothing, OtherType) => C): TypedList[C, Zero.type] = this

  override def concat[OtherType >: Nothing, OtherSize <: Natural](that: TypedList[OtherType, OtherSize]): TypedList[OtherType, OtherSize] = that

  override def flatMap[OtherType, OtherSize <: Natural](f: Nothing => TypedList[OtherType, OtherSize]): TypedList[OtherType, Zero.type] = this

  override def flatMap2[OtherType, OtherSize <: Natural](f: Nothing => TypedList[OtherType, OtherSize]): TypedList[OtherType, Zero.type] = this
  override private [typedlist] def _flatMap2Internal[OtherType, OtherSize <: Natural](f: Nothing => TypedList[OtherType, OtherSize]): TypedList[OtherType, Zero.type] = this

  override def :+[A1 >: Nothing](elem: A1): TypedList[A1, Suc[Zero.type]] = TypedCons(elem, TypedNil)

  override def ::[A1 >: Nothing](elem: A1): TypedList[A1, Suc[Zero.type]] = TypedCons(elem, TypedNil)

  override def reverse: TypedList[Nothing, Zero.type] = this

  override def split[At <: Suc[_ <: Natural]](
    implicit guaranteeIndexWithinBounds: LowerOrEqual[At, Zero.type],
    at: At,
    before: TypedList[Natural, At],
    after: TypedList[Natural, Zero.type]): (TypedList[Nothing, At], TypedList[Nothing, Zero.type]) =
    throw new Exception("Boom!")


  override private [typedlist] def _get(goalIndex: Natural): Nothing =
    throw new Exception("Boom!")

  override def obtainElementAsString: Option[String] = None

  override def count(predicate: Nothing => Boolean): Int = 0

  override def contains[A >: Nothing](elem: A): Boolean = false
}



case class TypedCons[Element, Size <: Natural](
  override protected val _head: Element,
  override protected val _tail: TypedList[Element, Size]
) extends TypedList[Element, Suc[Size]] {

  override def map[OtherType](f: Element => OtherType): TypedList[OtherType, Suc[Size]] = TypedCons(f(head), tail.map(f))

  override def zip[OtherType, C](that: TypedList[OtherType, Suc[Size]], f: (Element, OtherType) => C): TypedList[C, Suc[Size]] = that match {
    case TypedCons(thatHead, thatTail) => TypedCons(f(head, thatHead), tail.zip(thatTail, f))
  }
  override def zip2[OtherType, C](that: TypedList[OtherType, Suc[Size]], f: (Element, OtherType) => C): TypedList[C, Suc[Size]] =
    TypedCons(f(head, that.head), tail.zip2(that.tail, f))

  override def concat[OtherType >: Element, OtherSize <: Natural](that: TypedList[OtherType, OtherSize]): TypedList[OtherType, Suc[Size#Plus[OtherSize]]] =
    TypedCons(head, tail.concat(that))

  override def flatMap[OtherType, OtherSize <: Natural](f: Element => TypedList[OtherType, OtherSize]): TypedList[OtherType, OtherSize#Plus[Size#Mult[OtherSize]]] =
    f(head) concat tail.flatMap(f)

  override def flatMap2[OtherType, OtherSize <: Natural](f: Element => TypedList[OtherType, OtherSize]): TypedList[OtherType, Size#Mult2[OtherSize]#Plus[OtherSize]] =
    _flatMap2Internal(f).reverse
  override private [typedlist] def _flatMap2Internal[OtherType, OtherSize <: Natural](f: Element => TypedList[OtherType, OtherSize]): TypedList[OtherType, Size#Mult2[OtherSize]#Plus[OtherSize]] =
    tail._flatMap2Internal(f) concat f(head)

  override def reverse: TypedList[Element, Suc[Size]] = tail.reverse :+ head

  override def :+[A1 >: Element](elem: A1): TypedCons[A1, Suc[Size]] = TypedCons(head, tail.:+(elem))

  override def ::[A1 >: Element](elem: A1): TypedList[A1, Suc[Suc[Size]]] = TypedCons(elem, this)

  override def split[At <: Suc[_ <: Natural]](
    implicit guaranteeIndexWithinBounds: At LowerOrEqual Size,
    at: At,
    before: TypedList[Natural, At],
    after: TypedList[Natural, Suc[Size]#Minus[At]]): (TypedList[Element, At], TypedList[Element,  Suc[Size]#Minus[At]]) =
    (
    before.map(_get),
    after.map(i => _get(i.plus(at))))

  override private [typedlist] def _get(goalIndex: Natural): Element = {
    if ( goalIndex == Suc(Zero) ){
      head
    } else {
      tail._get(goalIndex.previous)
    }
  }

  override def obtainElementAsString: Option[String] = Some(tail.obtainElementAsString.fold(s"$head")(s"$head, " + _))

  override def count(predicate: Element => Boolean): Int =
    if (predicate(head))  1 + tail.count(predicate) else tail.count(predicate)

  override def contains[A >: Element](elem: A): Boolean =
    head == elem || tail.contains(elem)
}

object TypedList {

  implicit def typedListOfNats[N <: Natural](implicit previousNatTypedList: TypedList[Natural, N], thisNat: Suc[N]): TypedList[Natural, Suc[N]] =
    previousNatTypedList :+ thisNat

  implicit def emptyList[A]: TypedList[A, Zero.type] = TypedNil
}