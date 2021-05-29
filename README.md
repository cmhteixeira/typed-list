# Typed List &emsp; [![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.cmhteixeira/typed-list_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.cmhteixeira/typed-list_2.12) [![Build Status](https://www.travis-ci.com/cmhteixeira/typed-list.svg?branch=master)](https://www.travis-ci.com/cmhteixeira/typed-list)

Typed List is a Scala linked list with **typed** size. 

## Intro
Blog post on the development [here](https://aerodatablog.wordpress.com/2019/03/03/a-typedlist-in-scala/)

A typed list in Scala: a list whose size is known at compile time.  
No 3rd party libraries. 

## Usage

```scala
val aListOfSizeEight = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: TypedNil

// Compile error when accessing element outside bounds (as the bounds are known at compile time)
// aListOfSizeEight.get[Nat10] // uncomment line

// Compile error when accessing head/tail of empty list 
val aListOfSizeOne = "Carlos" :: TypedNil
val anEmptyList = aListOfSizeOne.tail
println(anEmptyList)
//println(anEmptyList.head)  //uncomment line
//println(anEmptyList.tail)  //uncomment line

// You can get an element at a specific index
val elemAtIndex = aListOfSizeEight.get[Nat3]
println(elemAtIndex)


// You can split into two lists mantaining typed size on both.
val (ofSize2, ofSize6) = aListOfSizeEight.split[Nat2]
println(ofSize2)
println(ofSize6)

// You can map over the list
val stringList = aListOfSizeEight.map(i => s"cmhteixeira-$i")
println(stringList)

// You can concatenate two lists and the resulting list has correct typed size
val firstList = "Foo" :: "Bar" :: "Baz" :: TypedNil
val secondList = "Qux" :: "Quux" :: TypedNil
val concatenatedList = firstList concat secondList
println(concatenatedList)
println(concatenatedList.get[Nat5])
// println(concatenatedList.get[Nat6]) // uncomment line

// You can flatmap the list, and the resulting list will have the correct size !! -> natural multiplication
val someList = "Foo" :: "Bar" :: "Baz" :: TypedNil
val result = someList.flatMap(i => s"$i-1" :: s"$i-2" :: TypedNil)
println(result)
println(result.get[Nat6])
// println(result.get[Nat7])  // uncomment line

```