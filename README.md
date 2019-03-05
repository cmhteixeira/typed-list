## Intro
Blog post on the development [here](https://aerodatablog.wordpress.com/2019/03/03/a-typedlist-in-scala/)

A typed list in Scala: a list whose size is known at compile time.  
No 3rd party libraries. 

## Usage

```scala
val foo = 11 :: 20 :: 34 :: 49 :: 54 :: TypedNil

foo.split[Nat2]
// res: (TypedList[Int, Nat2], TypedList[Int, Nat3]) = (TypedList(11, 20), TypedList(34, 49, 54) )

foo.get[Nat4]
// res: Int = 49

foo.get[Nat6]
// error: Nat6 is not lower or equal to Nat5

val bar: TypedList[Int, Nat2] = 1000 :: 2000 :: TypedNil
foo concat bar
// res: TypedList[Int, Nat7] = TypedList(11, 20, 34, 49, 54, 1000, 2000)

foo.map(elem => s"Foo-" + elem)
// res: TypedList[String, Nat5] = TypedList(Foo-11,Foo-20,Foo-34,Foo-49,Foo-54)

```