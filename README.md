# Typed List &emsp; [![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.cmhteixeira/typed-list_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.cmhteixeira/typed-list_2.12) [![Build Status](https://www.travis-ci.com/cmhteixeira/typed-list.svg?branch=master)](https://www.travis-ci.com/cmhteixeira/typed-list)

A linked list with compile time size.

**Found the project interesting?** &rightarrow;  Then drop a :star:. I am needing the encouragement. ;)  
**Found a problem?** &rightarrow; Open an issue, and I will look into it ASAP.  
**Want to contribute?** &rightarrow; If you found a bug and want to fix it yourself open an MR.  I welcome contributions.

---
**📝 NOTE - No large lists.**

In practise, this library will only work for relatively small lists. (I couldn't go past 400 elements).  
The problem is that it relies on the peano encoding of the natural numbers to express the size of the lists. This is very heavy on the compiler and it simply gives up.  
I think there is a presentation somewhere of Miles Sabin reporting the same issue in his Shapeless library.

---

## Usage

Create a typed list the same way you would a standard list:
```scala
import com.cmhteixeira.typedlist.TypedNil
val aListOfSizeEight = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: TypedNil

val standardList = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil
```
You can also use a standard list by specifying the size you expect. The result is wrapped in an `Option`.
```scala
import com.cmhteixeira.typedlist.TypedList
import com.cmhteixeira.typedlist.naturalnumbers.Natural.{Nat10, Nat5}

val standardList= (1 to 50).toList
val typedList = TypedList.fromList[Nat10#Mult[Nat5], Int](standardList)
```

Because the size is known at compile time, you cannot access the head or tail of an empty list:
```scala
val oneElement = "Foo" :: TypedNil
oneElement.tail.head // does not compile
oneElement.tail.tail // does not compile
```

You can obtain an element at a given index:
```scala
import com.cmhteixeira.typedlist.TypedNil
import com.cmhteixeira.typedlist.naturalnumbers.Natural.Nat2
val typedList = "Foo" :: "Bar" :: "Baz" :: TypedNil
typedList.get[Nat2]
res> "Bar"  
```
But you cannot obtain an element at an index greater that the size of the list:
```scala
import com.cmhteixeira.typedlist.TypedNil
import com.cmhteixeira.typedlist.naturalnumbers.Natural.Nat4
val typedList = "Foo" :: "Bar" :: "Baz" :: TypedNil
typedList.get[Nat4] // does not compile
```

You can split two lists at a given index, and the resulting lists will maintain type information. That is, their size will still be known:
```scala
import com.cmhteixeira.typedlist.TypedNil
import com.cmhteixeira.typedlist.naturalnumbers.Natural.Nat3
val typedList = "Foo" :: "Bar" :: "Baz" :: "Qux" :: "Quux" :: TypedNil
val (l, r) = typedList.split[Nat3]
res> 
val l: TypedList[Suc[Suc[Suc[Zero.type]]], String] = TypedList("Foo", "Bar", "Baz")
val r: TypedList[Suc[Suc[Zero.type]], String] = TypedList("Qux", "Quux")
```

You can also map over the list, and the resulting list will maintain its size:
```scala
import com.cmhteixeira.typedlist.TypedNil
val typedList = "Bar" :: "Baz" :: TypedNil
val stringList = aListOfSizeEight.map(_.length)
res> 
val stringList = TypedList(3, 3)
```

You can concatenate two lists and the resulting list will still have the correct typed size; which in fact will be a summation of the sizes of the two lists being concatenated:
```scala
import com.cmhteixeira.typedlist.TypedNil
val firstList = "Foo" :: "Bar" :: TypedNil
val secondList = "Baz" :: TypedNil
val concatenatedList = firstList concat secondList
concatenatedList.tail.tail.tail.head // will not compile
concatenatedList.tail.tail.tail.tail // will not compile
res>
val concatenatedList = TypedList(Foo, Bar, Baz)
```

Lastly, you can flatmap a list, and the resulting list will have the correct size which will still be known at compile time. The drawback is that the function that is applied to each element of the original list must return a new list with a constant size. That is, the returning size cannot vary across the elements of the original list:

```scala
import com.cmhteixeira.typedlist.TypedNil
import com.cmhteixeira.typedlist.naturalnumbers.Natural.Nat6

val someList = "Foo" :: "Bar" :: "Baz" :: TypedNil
val result = someList.flatMap(i => s"$i-1" :: s"$i-2" :: TypedNil)
res> 
val result: TypedList[Nat6, String] = TypedList(Foo-1, Foo-2, Bar-1, Bar-2, Baz-1, Baz-2)
```

### Cats support

A Typed list has a `Traverse` instance. Which also makes it a `Functor` and a `Foldable`.  
You **don't need** to import any implicits to have it working:  
```scala
import cats.syntax.traverse._
import cats.implicits.catsStdInstancesForOption
import com.cmhteixeira.typedlist.TypedNil

val list =  1 :: 2 :: 3 :: 4 :: TypedNil

list.traverse[Option, Int](i => Some(i))
res> Some(TypedList(1, 2, 3, 4))

list.traverse[Option, Int](i => if (i == 2) None else Some(i))
res> None
```

## Support

The artefacts have been uploaded to Maven Central.

| Library Version | Scala 2.11 | Scala 2.12 | Scala 2.13 |
|---------|------------|------------|------------|
| 1.0.0   | | [![Maven Central](https://img.shields.io/maven-central/v/com.cmhteixeira/typed-list_2.12/1.0.0)](https://search.maven.org/artifact/com.cmhteixeira/typed-list_2.12/1.0.0/jar)        | |
| 0.1   | | [![Maven Central](https://img.shields.io/maven-central/v/com.cmhteixeira/typed-list_2.12/0.1)](https://search.maven.org/artifact/com.cmhteixeira/typed-list_2.12/0.1/jar)        | |
