<!-- wp:heading {"level":3} -->
<h3>Introduction</h3>
<!-- /wp:heading -->

<!-- wp:paragraph -->
<p>Lets try and write a typed list in Scala: a list whose size is known at compile time.</p>
<!-- /wp:paragraph -->

<!-- wp:paragraph -->
<p>While the idea is not exactly new, there aren't so many tutorials on how to go about this. Above all though, it is a great excuse to practice with the type system, implicits and recursion.   </p>
<!-- /wp:paragraph -->

<!-- wp:jetpack/markdown {"source":"So, a typed list, besides being parameterized by the type of elements it holds, like the common `List[+A]`, it is also parameterized by its size as in `TypedList[+A, Size]`.  \nThis means there are some verifications that can be transferred from run-time to compile-time - basically, everything related with list size. Some examples:  "} -->
<div class="wp-block-jetpack-markdown"><p>So, a typed list, besides being parameterized by the type of elements it holds, like the common <code>List[+A]</code>, it is also parameterized by its size as in <code>TypedList[+A, Size]</code>.<br>
This means there are some verifications that can be transferred from run-time to compile-time - basically, everything related with list size. Some examples:</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:table {"align":"wide","className":"is-style-regular"} -->
<table class="wp-block-table alignwide is-style-regular"><tbody><tr><td></td><td><strong>Normal List</strong></td><td><strong>Typed List</strong></td></tr><tr><td>EmptyList.<strong>head&nbsp;</strong>along with <br><strong>max,&nbsp;min,&nbsp;last</strong> (on EmptyList)</td><td>Run-time exception</td><td>Does not compile</td></tr><tr><td>ListA(index)<br><em>i.e</em> <em>get element at index</em></td><td>Run-time IndexOutOfBoundsException<br>if index &gt; than list size (or  index &lt; 0)</td><td>Does not compile</td></tr><tr><td>listA <strong>zip </strong>listB</td><td>Drops extra elements of bigger list<br>List(1, 2, 3) zip List(10, 20) <br>// res: List(  (1, 10),  (2, 20)  )</td><td>Does not compile if lists are of  ≠ size  </td></tr><tr><td>listA <strong>splitAt </strong>index</td><td>Returns an empty list when <br>index is out of bounds<br>List(1, 2, 3).splitAt(4)<br>// res: (  List(1, 2, 3) , List() )</td><td>Does not compile if index is &gt; then the<br>list size</td></tr><tr><td>listA <strong>filter</strong> predicate</td><td>Possible</td><td>Impossible (unless we return a normal list)</td></tr></tbody></table>
<!-- /wp:table -->

<!-- wp:jetpack/markdown {"source":"Notice there are some operations impossible to construct on typed lists, or rather, which will invariably have a different \u0022api\u0022. For example, we can not expect to return a typed list on the filter method as the filtering is done at run-time and there is no information at compile-time of how many elements would satisfy the predicate. The signature on filter must therefore return a normal list. "} -->
<div class="wp-block-jetpack-markdown"><p>Notice there are some operations impossible to construct on typed lists, or rather, which will invariably have a different &quot;api&quot;. For example, we can not expect to return a typed list on the filter method as the filtering is done at run-time and there is no information at compile-time of how many elements would satisfy the predicate. The signature on filter must therefore return a normal list.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"At the end the api should be something along:\n```\nval foo: TypedList[Int, Nat5] = 11 :: 20 :: 34 :: 49 :: 54 :: TypedNil\n\nfoo.split[Nat2]\n// res: (TypedList[Int, Nat2], TypedList[Int, Nat3]) = (TypedList(11, 20), TypedList(34, 49, 54) )\n\nfoo.get[Nat4]\n// res: Int = 49\n\nval bar: TypedList[Int, Nat2] = 1000 :: 2000 :: TypedNil\nfoo concat bar\n// res: TypedList[Int, Nat7] = TypedList(11, 20, 34, 49, 54, 1000, 2000)\n\nfoo.map(elem =\u003e s\u0022Foo-\u0022 + elem)\n// res: TypedList[String, Nat5] = TypedList(Foo-11,Foo-20,Foo-34,Foo-49,Foo-54)\n```"} -->
<div class="wp-block-jetpack-markdown"><p>At the end the api should be something along:</p>
<pre><code>val foo: TypedList[Int, Nat5] = 11 :: 20 :: 34 :: 49 :: 54 :: TypedNil

foo.split[Nat2]
// res: (TypedList[Int, Nat2], TypedList[Int, Nat3]) = (TypedList(11, 20), TypedList(34, 49, 54) )

foo.get[Nat4]
// res: Int = 49

val bar: TypedList[Int, Nat2] = 1000 :: 2000 :: TypedNil
foo concat bar
// res: TypedList[Int, Nat7] = TypedList(11, 20, 34, 49, 54, 1000, 2000)

foo.map(elem =&gt; s&quot;Foo-&quot; + elem)
// res: TypedList[String, Nat5] = TypedList(Foo-11,Foo-20,Foo-34,Foo-49,Foo-54)
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:heading {"level":3} -->
<h3>The Natural numbers</h3>
<!-- /wp:heading -->

<!-- wp:jetpack/markdown {"source":"As the size of the list, it being the number of elements it holds, can only be 0 or greater and since we want to parameterize a list on its size, then the 1st step is to encode the natural numbers at the type level; such that there is a type for every natural number. This will allow us to do something like:   \n```scala\nval foo: TypedList[String, Four] = \u0022Foo\u0022 :: \u0022Bar\u0022 :: \u0022Baz\u0022 :: \u0022Qux\u0022 :: TypedNil\n\n```"} -->
<div class="wp-block-jetpack-markdown"><p>As the size of the list, it being the number of elements it holds, can only be 0 or greater and since we want to parameterize a list on its size, then the 1st step is to encode the natural numbers at the type level; such that there is a type for every natural number. This will allow us to do something like:</p>
<pre><code class="language-scala">val foo: TypedList[String, Four] = &quot;Foo&quot; :: &quot;Bar&quot; :: &quot;Baz&quot; :: &quot;Qux&quot; :: TypedNil

</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"The following is a pivotal step of the whole process. The baseline was the encoding of natural numbers as presented [here](#joe_barnes_talk) and [here](#yao_li). It starts with:  \n```scala\nsealed trait Natural\n\ncase object Zero extends Natural\ncase class Suc[N \u003c: Natural]() extends Natural\n```\nThe following diagram captures the type hierarchy. "} -->
<div class="wp-block-jetpack-markdown"><p>The following is a pivotal step of the whole process. The baseline was the encoding of natural numbers as presented <a href="#joe_barnes_talk">here</a> and <a href="#yao_li">here</a>. It starts with:</p>
<pre><code class="language-scala">sealed trait Natural

case object Zero extends Natural
case class Suc[N &lt;: Natural]() extends Natural
</code></pre>
<p>The following diagram captures the type hierarchy.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:image {"id":615} -->
<figure class="wp-block-image"><img src="https://aerodatablog.files.wordpress.com/2018/12/NaturalNumbersTypeHiearchy.png" alt="" class="wp-image-615"/><figcaption>Fig.1 - Type hierarchy of the encoding of natural numbers</figcaption></figure>
<!-- /wp:image -->

<!-- wp:jetpack/markdown {"source":"This encoding is recursive. `Suc` (as in successor) is a type constructor that must take a concrete **natural** type to \u0022construct\u0022 yet another **natural** type.  \nThis infinite loop can only start at `Zero` as `Zero.type` is the only **concrete** natural (and so it can be passed to `Suc`) that is not build from a previous one.\n"} -->
<div class="wp-block-jetpack-markdown"><p>This encoding is recursive. <code>Suc</code> (as in successor) is a type constructor that must take a concrete <strong>natural</strong> type to &quot;construct&quot; yet another <strong>natural</strong> type.<br>
This infinite loop can only start at <code>Zero</code> as <code>Zero.type</code> is the only <strong>concrete</strong> natural (and so it can be passed to <code>Suc</code>) that is not build from a previous one.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"Throughout the post we will be using aliases for the natural types:  \n```  \n\ntype Nat0 = Zero.type  \ntype Nat1 = Suc[Nat0]\ntype Nat2 = Suc[Nat1]\ntype Nat3 = Suc[Nat2]  \n...\n```"} -->
<div class="wp-block-jetpack-markdown"><p>Throughout the post we will be using aliases for the natural types:</p>
<pre><code>
type Nat0 = Zero.type  
type Nat1 = Suc[Nat0]
type Nat2 = Suc[Nat1]
type Nat3 = Suc[Nat2]  
...
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"This is cleaner and more intelligible than using `Suc[Suc[Suc[Suc[Suc[Suc[Suc[.....` to represent some number.     \nOne might wonder the advantage of using such a fancy formulation if we need to manually type every single number.   \nI simpler formulation would be just: \n```  \n\nsealed trait Natural\ncase object Zero extends Natural\ncase object One extends Natural\ncase object Two extends Natural\ncase object Three extends Natural\n....\n```\nBut notice that in this formulation the numbers have no relation between themselves. \nWe would be unable to sum, subtract or multiply two numbers together as we will see further down. These operations with naturals being essential for a generic formulation. \n"} -->
<div class="wp-block-jetpack-markdown"><p>This is cleaner and more intelligible than using <code>Suc[Suc[Suc[Suc[Suc[Suc[Suc[.....</code> to represent some number.<br>
One might wonder the advantage of using such a fancy formulation if we need to manually type every single number.<br>
I simpler formulation would be just:</p>
<pre><code>
sealed trait Natural
case object Zero extends Natural
case object One extends Natural
case object Two extends Natural
case object Three extends Natural
....
</code></pre>
<p>But notice that in this formulation the numbers have no relation between themselves.
We would be unable to sum, subtract or multiply two numbers together as we will see further down. These operations with naturals being essential for a generic formulation.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:heading -->
<h2>Raw signature of the TypedList</h2>
<!-- /wp:heading -->

<!-- wp:jetpack/markdown {"source":"With the above we now have:  \n```scala\nsealed trait TypedList[+Element, Size \u003c: Natural]\n```\n\nWhere we have co-variance with `Element` as in a normal list.  \nHaving co-variance on `Size` as well was my first approach, but it lead to an array of problems downstream as seen on this SO [post](https://stackoverflow.com/questions/53708117/inference-of-underscore-types) . I settled with invariance as [Joe Barnes](#joe_barnes_talk) (minute 24:40).    "} -->
<div class="wp-block-jetpack-markdown"><p>With the above we now have:</p>
<pre><code class="language-scala">sealed trait TypedList[+Element, Size &lt;: Natural]
</code></pre>
<p>Where we have co-variance with <code>Element</code> as in a normal list.<br>
Having co-variance on <code>Size</code> as well was my first approach, but it lead to an array of problems downstream as seen on this SO <a href="https://stackoverflow.com/questions/53708117/inference-of-underscore-types">post</a> . I settled with invariance as <a href="#joe_barnes_talk">Joe Barnes</a> (minute 24:40).</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"```scala\nsealed trait TypedList[+Element, Size \u003c: Natural]{\n  def head: Element\n}\n\ncase object TypedNil extends TypedList[Nothing, Zero.type] {\n  override val head: Nothing = throw new Exception(\u0022Boom!\u0022)\n}\n\ncase class TypedCons[Element, N \u003c: Natural](\n  head: Element,\n  tail: TypedList[Element, N]\n) extends TypedList[Element, Suc[N]]\n\n```"} -->
<div class="wp-block-jetpack-markdown"><pre><code class="language-scala">sealed trait TypedList[+Element, Size &lt;: Natural]{
  def head: Element
}

case object TypedNil extends TypedList[Nothing, Zero.type] {
  override val head: Nothing = throw new Exception(&quot;Boom!&quot;)
}

case class TypedCons[Element, N &lt;: Natural](
  head: Element,
  tail: TypedList[Element, N]
) extends TypedList[Element, Suc[N]]

</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"The following is another **fundamental** concept:    \n`TypedCons[_, N]` extends `TypedList[_, Suc[N]]`.  \nWhy ?  \nWouldn't it be more intuitive to extend `TypedList[_, N]` instead ?  \nBecause the tail's size must be one less than the current list!  \nWith the current formulation, a `TypedCons[_, Nat4]` is a `TypedList[_, Nat5/*(= Suc[Nat4])*/]` with a tail of size `Nat4`. Very smart ! \nIf the extension was on `TypedList[_, N]` how would we encode that property?\n"} -->
<div class="wp-block-jetpack-markdown"><p>The following is another <strong>fundamental</strong> concept:<br>
<code>TypedCons[_, N]</code> extends <code>TypedList[_, Suc[N]]</code>.<br>
Why ?<br>
Wouldn't it be more intuitive to extend <code>TypedList[_, N]</code> instead ?<br>
Because the tail's size must be one less than the current list!<br>
With the current formulation, a <code>TypedCons[_, Nat4]</code> is a <code>TypedList[_, Nat5/*(= Suc[Nat4])*/]</code> with a tail of size <code>Nat4</code>. Very smart !
If the extension was on <code>TypedList[_, N]</code> how would we encode that property?</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"Continuing, notice we are still able to access the head of an empty list:\n```scala\n\nval emptyList = TypedNil\nval foo = emptyList.head       // compiles and \njava.lang.Exception: Boom!     // blows-up at compile time\n```"} -->
<div class="wp-block-jetpack-markdown"><p>Continuing, notice we are still able to access the head of an empty list:</p>
<pre><code class="language-scala">
val emptyList = TypedNil
val foo = emptyList.head       // compiles and 
java.lang.Exception: Boom!     // blows-up at compile time
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"But now we are powerful enough to to solve this; with the help of [phantom types](https://blog.codecentric.de/en/2016/02/phantom-types-scala/):\n```scala\nsealed trait TypedList[+Element, Size \u003c: Natural]{\n  def head[PhantomType \u003e: Size \u003c: Suc[_ \u003c: Natural]: Element\n}\n```\n`PhatomType` (name irrelevant, could be `BlaBla`) above exists for a single purpose: To encode a type constraint.  \nWe demand the compiler to find a type (the `PhatomType`) such that it must have `Size`( of the list) as lower-bound and `Suc[_ \u003c: Natural]` as upper-bound. With the help of figure 1 you will conclude this is impossible if `Size` is `Zero.type` as in the `TypedNil` case; Now the compiler fails with:  \n```\nval emptyList = TypedNil\nval foo = emptyList.head     //Does not compile with error:  \n\nerror: type arguments [Zero.type] do not conform to method head's type parameter bounds [PhantomType \u003e: Zero.type \u003c: Suc[_ \u003c: Natural]]\n       TypedNil.head\n                ^\n"} -->
<div class="wp-block-jetpack-markdown"><p>But now we are powerful enough to to solve this; with the help of <a href="https://blog.codecentric.de/en/2016/02/phantom-types-scala/">phantom types</a>:</p>
<pre><code class="language-scala">sealed trait TypedList[+Element, Size &lt;: Natural]{
  def head[PhantomType &gt;: Size &lt;: Suc[_ &lt;: Natural]: Element
}
</code></pre>
<p><code>PhatomType</code> (name irrelevant, could be <code>BlaBla</code>) above exists for a single purpose: To encode a type constraint.<br>
We demand the compiler to find a type (the <code>PhatomType</code>) such that it must have <code>Size</code>( of the list) as lower-bound and <code>Suc[_ &lt;: Natural]</code> as upper-bound. With the help of figure 1 you will conclude this is impossible if <code>Size</code> is <code>Zero.type</code> as in the <code>TypedNil</code> case; Now the compiler fails with:</p>
<pre><code>val emptyList = TypedNil
val foo = emptyList.head     //Does not compile with error:  

error: type arguments [Zero.type] do not conform to method head's type parameter bounds [PhantomType &gt;: Zero.type &lt;: Suc[_ &lt;: Natural]]
       TypedNil.head
                ^
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:heading -->
<h2>The .map and .zip operations</h2>
<!-- /wp:heading -->

<!-- wp:jetpack/markdown {"source":"This are the simplest methods to implement and do not require any reasoning more advanced than their counterparts in a normal list."} -->
<div class="wp-block-jetpack-markdown"><p>This are the simplest methods to implement and do not require any reasoning more advanced than their counterparts in a normal list.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:heading {"level":4} -->
<h4>The .map</h4>
<!-- /wp:heading -->

<!-- wp:jetpack/markdown {"source":"We are interested in developing a `.map` method that should have the same meaning as in a normal list: apply the same function to every element. In our case, the returning list should have the same length as the original:\n```scala\nsealed trait TypedList[+Element, Size \u003c: Natural] {\n  def map[OtherType](f: Element =\u003e OtherType): TypedList[OtherType, Size]\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><p>We are interested in developing a <code>.map</code> method that should have the same meaning as in a normal list: apply the same function to every element. In our case, the returning list should have the same length as the original:</p>
<pre><code class="language-scala">sealed trait TypedList[+Element, Size &lt;: Natural] {
  def map[OtherType](f: Element =&gt; OtherType): TypedList[OtherType, Size]
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":" ```\ncase object TypedNil extends TypedList[Nothing, Zero.type] {\n    override def map[OtherType](f: Nothing =\u003e OtherType): TypedList[Nothing, Zero.type] = this\n}  \n```\n\n```\ncase class TypedCons[Element, Size \u003c: Natural](\n  override val _head: Element,\n  tail: TypedList[Element, Size]\n) extends TypedList[Element, Suc[Size]] {\n  override def map[OtherType](f: Element =\u003e OtherType): TypedList[OtherElement, Suc[Size]] = TypedCons(f(head), tail.map(f))\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><pre><code>case object TypedNil extends TypedList[Nothing, Zero.type] {
   override def map[OtherType](f: Nothing =&gt; OtherType): TypedList[Nothing, Zero.type] = this
}  
</code></pre>
<pre><code>case class TypedCons[Element, Size &lt;: Natural](
  override val _head: Element,
  tail: TypedList[Element, Size]
) extends TypedList[Element, Suc[Size]] {
  override def map[OtherType](f: Element =&gt; OtherType): TypedList[OtherElement, Suc[Size]] = TypedCons(f(head), tail.map(f))
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:heading {"level":4} -->
<h4>The .zip</h4>
<!-- /wp:heading -->

<!-- wp:jetpack/markdown {"source":"This case is a slightly more interesting.  \nSo, you have two lists and you want to perform an operation on every pair - each element (of the pair) being provided by a different list but at the same index.  \nOn a normal list you cannot enforce the two lists to have the same size; when one is longer, its additional elements are ignored:  \n```scala\n\nval foo = List(1, 2, 3, 4)\nval bar = List(1, 2, 3)\n\nfoo.zip(bar)\n//res: List((1,1), (2,2), (3,3))\n```"} -->
<div class="wp-block-jetpack-markdown"><p>This case is a slightly more interesting.<br>
So, you have two lists and you want to perform an operation on every pair - each element (of the pair) being provided by a different list but at the same index.<br>
On a normal list you cannot enforce the two lists to have the same size; when one is longer, its additional elements are ignored:</p>
<pre><code class="language-scala">
val foo = List(1, 2, 3, 4)
val bar = List(1, 2, 3)

foo.zip(bar)
//res: List((1,1), (2,2), (3,3))
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"Now, not only can we enforce that, it comes out as very natural.  \nOn the signature at `TypedList`, `zip` must receive another list whose size must be the same as the current list:  "} -->
<div class="wp-block-jetpack-markdown"><p>Now, not only can we enforce that, it comes out as very natural.<br>
On the signature at <code>TypedList</code>, <code>zip</code> must receive another list whose size must be the same as the current list:</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"```scala\nsealed trait TypedList[+Element, Size \u003c: Natural]{\n  def zip[OtherType, C](that: TypedList[OtherType, Size], f: (Element, OtherType) =\u003e C): TypedList[C, Size]\n}\n```\n```scala\ncase object TypedNil extends TypedList[Nothing, Zero.type]{\n  override def zip[OtherType, C](that: TypedList[OtherType, Zero.type], f: (Nothing, OtherType) =\u003e C): TypedList[C, Zero.type] = this\n}\n```\n```scala\ncase class TypedCons[Element, Size \u003c: Natural](\n  override val _head: Element,\n  tail: TypedList[Element, Size]\n) extends TypedList[Element, Suc[Size]] {\n  override def zip[OtherType, C](that: TypedList[OtherType, Suc[Size]], f: (Element, OtherType) =\u003e C): TypedList[C, Suc[Size]] = that match {\n    case TypedCons(thatHead, thatTail) =\u003e TypedCons(f(head, thatHead), tail.zip(thatTail, f))\n  }\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><pre><code class="language-scala">sealed trait TypedList[+Element, Size &lt;: Natural]{
  def zip[OtherType, C](that: TypedList[OtherType, Size], f: (Element, OtherType) =&gt; C): TypedList[C, Size]
}
</code></pre>
<pre><code class="language-scala">case object TypedNil extends TypedList[Nothing, Zero.type]{
  override def zip[OtherType, C](that: TypedList[OtherType, Zero.type], f: (Nothing, OtherType) =&gt; C): TypedList[C, Zero.type] = this
}
</code></pre>
<pre><code class="language-scala">case class TypedCons[Element, Size &lt;: Natural](
  override val _head: Element,
  tail: TypedList[Element, Size]
) extends TypedList[Element, Suc[Size]] {
  override def zip[OtherType, C](that: TypedList[OtherType, Suc[Size]], f: (Element, OtherType) =&gt; C): TypedList[C, Suc[Size]] = that match {
    case TypedCons(thatHead, thatTail) =&gt; TypedCons(f(head, thatHead), tail.zip(thatTail, f))
  }
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"**An interesting detour**  \nAbove, we must pattern match the `that` list on the `TypedCons` implementation. Not only that, but that match has only 1 case which is exhaustive! How come ?  \nWe must pattern match because the list `that` is a `TypedList` and there is no `tail` defined there; only on `TypedCons`. At the same time, we need to have access to the `tail` in order to achieve the semantics of `zip` via recursion.  \nOne would think that we would then have to specify logic for the 2 cases (`TypedNil` and `TypedCons`). Not necessary! since the compiler knows that a `TypedList[OtherType, Suc[Size]]` can never be `TypedNil` for any given `Size \u003c: Natural`. \nThis is kinda funny behavior from the compiler. \n\n\n\n"} -->
<div class="wp-block-jetpack-markdown"><p><strong>An interesting detour</strong><br>
Above, we must pattern match the <code>that</code> list on the <code>TypedCons</code> implementation. Not only that, but that match has only 1 case which is exhaustive! How come ?<br>
We must pattern match because the list <code>that</code> is a <code>TypedList</code> and there is no <code>tail</code> defined there; only on <code>TypedCons</code>. At the same time, we need to have access to the <code>tail</code> in order to achieve the semantics of <code>zip</code> via recursion.<br>
One would think that we would then have to specify logic for the 2 cases (<code>TypedNil</code> and <code>TypedCons</code>). Not necessary! since the compiler knows that a <code>TypedList[OtherType, Suc[Size]]</code> can never be <code>TypedNil</code> for any given <code>Size &lt;: Natural</code>.
This is kinda funny behavior from the compiler.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"But could we take out the pattern match? Yes.  \nThe difficulty is in defining a tail on the trait `TypedList` since we must restrict it (the tail) to have size 1 less than the `TypedList`. \nRecall for the `TypedCons` case this was embedded in the definition of `TypedCons` itself and the way it extended `TypedList`.    \nIn other words, what should we substitute `???` below for ? \n```scala\nsealed trait TypedList[+Element, Size \u003c: Natural] {\n  def head: Element\n  def tail: TypedList[Element, ???]\n}\n```\nThe answer lies in defining a type `Previous` for every `Natural` type. \n\n```scala\nsealed trait Natural {\n  type Previous \u003c: Natural\n}\n\ncase object Zero extends Natural {\n  override type Previous = Zero.type\n}\n\ncase class Suc[N \u003c: Natural]() extends Natural {\n  override type Previous = N\n}\n```\n"} -->
<div class="wp-block-jetpack-markdown"><p>But could we take out the pattern match? Yes.<br>
The difficulty is in defining a tail on the trait <code>TypedList</code> since we must restrict it (the tail) to have size 1 less than the <code>TypedList</code>.
Recall for the <code>TypedCons</code> case this was embedded in the definition of <code>TypedCons</code> itself and the way it extended <code>TypedList</code>.<br>
In other words, what should we substitute <code>???</code> below for ?</p>
<pre><code class="language-scala">sealed trait TypedList[+Element, Size &lt;: Natural] {
  def head: Element
  def tail: TypedList[Element, ???]
}
</code></pre>
<p>The answer lies in defining a type <code>Previous</code> for every <code>Natural</code> type.</p>
<pre><code class="language-scala">sealed trait Natural {
  type Previous &lt;: Natural
}

case object Zero extends Natural {
  override type Previous = Zero.type
}

case class Suc[N &lt;: Natural]() extends Natural {
  override type Previous = N
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"Which allows us then to do: "} -->
<div class="wp-block-jetpack-markdown"><p>Which allows us then to do:</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"```scala\nsealed trait TypedList[+Element, Size \u003c: Natural]{\n  protected def _tail: TypedList[Element, Size#Previous]\n  def tail[PhantomType \u003e: Size \u003c: Suc[_ \u003c: Natural]] = _tail\n}\n```\nWith now a tail on the super trait, we don't need the annoying pattern match on `that`:\n```scala\ncase class TypedCons[Element, Size \u003c: Natural](\n  override protected val _head: Element,\n  override protected val _tail: TypedList[Element, Size]\n) extends TypedList[Element, Suc[Size]] {\n  override def zip[OtherType, C](that: TypedList[OtherType, Suc[Size]], f: (Element, OtherType) =\u003e C): TypedList[C, Suc[Size]] =\n    TypedCons(f(head, that.head), tail.zip(that.tail, f))\n}"} -->
<div class="wp-block-jetpack-markdown"><pre><code class="language-scala">sealed trait TypedList[+Element, Size &lt;: Natural]{
  protected def _tail: TypedList[Element, Size#Previous]
  def tail[PhantomType &gt;: Size &lt;: Suc[_ &lt;: Natural]] = _tail
}
</code></pre>
<p>With now a tail on the super trait, we don't need the annoying pattern match on <code>that</code>:</p>
<pre><code class="language-scala">case class TypedCons[Element, Size &lt;: Natural](
  override protected val _head: Element,
  override protected val _tail: TypedList[Element, Size]
) extends TypedList[Element, Suc[Size]] {
  override def zip[OtherType, C](that: TypedList[OtherType, Suc[Size]], f: (Element, OtherType) =&gt; C): TypedList[C, Suc[Size]] =
    TypedCons(f(head, that.head), tail.zip(that.tail, f))
}</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:heading {"level":3} -->
<h3>The .concat method</h3>
<!-- /wp:heading -->

<!-- wp:jetpack/markdown {"source":"This is where things get more interesting.  \nThe `.concat` should give us a new list with the elements from the left followed by the elements of the right. Something like  \n```scala\nval foo: TypedList[String, Nat2] = \u0022Foo\u0022 :: \u0022Bar\u0022 :: TypedNil\nval bar: TypedList[String, Nat2] = \u0022Baz\u0022 :: \u0022Qux\u0022 :: TypedNil\n\nfoo.concat(bar)\n// res: TypedList[String, Nat4] = TypedList(Foo, Bar, Baz, Qux)\n```"} -->
<div class="wp-block-jetpack-markdown"><p>This is where things get more interesting.<br>
The <code>.concat</code> should give us a new list with the elements from the left followed by the elements of the right. Something like</p>
<pre><code class="language-scala">val foo: TypedList[String, Nat2] = &quot;Foo&quot; :: &quot;Bar&quot; :: TypedNil
val bar: TypedList[String, Nat2] = &quot;Baz&quot; :: &quot;Qux&quot; :: TypedNil

foo.concat(bar)
// res: TypedList[String, Nat4] = TypedList(Foo, Bar, Baz, Qux)
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"The resulting list should naturally also be typed and so the compiler must know how to sum two natural numbers:   \n``` \nsealed trait Natural {\n  type Plus[That \u003c: Natural] \u003c: Natural\n}\n```\nwhich is trivial for `Zero.type`:  \n```\ncase object Zero extends Natural {\n  override type Plus[That \u003c: Natural] = That\n}\n```\nIt took me a few minutes and pen and paper to convince myself that the recursive formulation embodies summation:  \n```\ncase class Suc[N \u003c: Natural]() extends Natural {\n  override type Plus[That \u003c: Natural] = Suc[N#Plus[That]]\n}\n```\nThe recursive loop (at the tyle level) eventually hits Zero.type, ending and returning the desired type.\n"} -->
<div class="wp-block-jetpack-markdown"><p>The resulting list should naturally also be typed and so the compiler must know how to sum two natural numbers:</p>
<pre><code>sealed trait Natural {
  type Plus[That &lt;: Natural] &lt;: Natural
}
</code></pre>
<p>which is trivial for <code>Zero.type</code>:</p>
<pre><code>case object Zero extends Natural {
  override type Plus[That &lt;: Natural] = That
}
</code></pre>
<p>It took me a few minutes and pen and paper to convince myself that the recursive formulation embodies summation:</p>
<pre><code>case class Suc[N &lt;: Natural]() extends Natural {
  override type Plus[That &lt;: Natural] = Suc[N#Plus[That]]
}
</code></pre>
<p>The recursive loop (at the tyle level) eventually hits Zero.type, ending and returning the desired type.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"With this we can the signature:  \n```\nsealed trait TypedList[+Element, Size \u003c: Natural]{\n  def concat[OtherElement \u003e: Element, OtherSize \u003c: Natural](that: TypedList[OtherElement, OtherSize]): TypedList[OtherElement, Size#Plus[OtherSize]]\n}\n```\nWith the following implementations:  \n```\ncase object TypedNil extends TypedList[Nothing, Zero.type]{\n  override def concat[OtherElement \u003e: Nothing, OtherSize \u003c: Natural](that: TypedList[OtherElement, OtherSize]): TypedList[OtherElement, OtherSize] = that\n}\n```\n```\ncase class TypedCons[Element, Size \u003c: Natural](\n  override protected val _head: Element,\n  override protected val _tail: TypedList[Element, Size]\n) extends TypedList[Element, Suc[Size]] {\n  override def concat[OtherElement \u003e: Element, OtherSize \u003c: Natural](that: TypedList[OtherElement, OtherSize]): TypedList[OtherElement, Suc[Size#Plus[OtherSize]]] =\n    TypedCons(head, tail.concat(that))\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><p>With this we can the signature:</p>
<pre><code>sealed trait TypedList[+Element, Size &lt;: Natural]{
  def concat[OtherElement &gt;: Element, OtherSize &lt;: Natural](that: TypedList[OtherElement, OtherSize]): TypedList[OtherElement, Size#Plus[OtherSize]]
}
</code></pre>
<p>With the following implementations:</p>
<pre><code>case object TypedNil extends TypedList[Nothing, Zero.type]{
  override def concat[OtherElement &gt;: Nothing, OtherSize &lt;: Natural](that: TypedList[OtherElement, OtherSize]): TypedList[OtherElement, OtherSize] = that
}
</code></pre>
<pre><code>case class TypedCons[Element, Size &lt;: Natural](
  override protected val _head: Element,
  override protected val _tail: TypedList[Element, Size]
) extends TypedList[Element, Suc[Size]] {
  override def concat[OtherElement &gt;: Element, OtherSize &lt;: Natural](that: TypedList[OtherElement, OtherSize]): TypedList[OtherElement, Suc[Size#Plus[OtherSize]]] =
    TypedCons(head, tail.concat(that))
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:heading {"level":3} -->
<h3>The .flatMap</h3>
<!-- /wp:heading -->

<!-- wp:jetpack/markdown {"source":"This method is useful to explain another operation on natural numbers: multiplication.  \nOn a `.flatMap`, we apply a function to every element of the left list, such that the result (for each element) is another list, flattening everything at the end.  \n```\nval foo: List[Int] = List(1, 2, 3)\ndef bar(i: Int): List[String] = i match {\n  case 1 =\u003e List(\u0022Foo\u0022)\n  case 2 =\u003e List(\u0022Bar\u0022, \u0022Baz\u0022)\n  case 3 =\u003e List(\u0022Qux\u0022, \u0022Quux\u0022, \u0022FooBar\u0022, \u0022BarBaz\u0022)\n  case _ =\u003e List()\n}\nfoo.flatMap(bar)\n//res: List[String] = List(Foo, Bar, Baz, Qux, Quux, FooBar, BarBaz)\n```\n"} -->
<div class="wp-block-jetpack-markdown"><p>This method is useful to explain another operation on natural numbers: multiplication.<br>
On a <code>.flatMap</code>, we apply a function to every element of the left list, such that the result (for each element) is another list, flattening everything at the end.</p>
<pre><code>val foo: List[Int] = List(1, 2, 3)
def bar(i: Int): List[String] = i match {
  case 1 =&gt; List(&quot;Foo&quot;)
  case 2 =&gt; List(&quot;Bar&quot;, &quot;Baz&quot;)
  case 3 =&gt; List(&quot;Qux&quot;, &quot;Quux&quot;, &quot;FooBar&quot;, &quot;BarBaz&quot;)
  case _ =&gt; List()
}
foo.flatMap(bar)
//res: List[String] = List(Foo, Bar, Baz, Qux, Quux, FooBar, BarBaz)
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"The above cannot be achieved on our typed list. The size of the lists being returned for each case depends on values! Therefore the resulting list' size could not be known at compile time.  \nWe can however, formulate a `.flatMap` such that each returned list is of a constant/pre-determined size. Whether or not that is useful for anything is another question.   \nBefore spoiling with the signature, understand that we need multiplication of naturals: If the original list has **n** elements, each of which is mapped to a list of **m** elements, the result after flattening shall have **n*m** elements.\n"} -->
<div class="wp-block-jetpack-markdown"><p>The above cannot be achieved on our typed list. The size of the lists being returned for each case depends on values! Therefore the resulting list' size could not be known at compile time.<br>
We can however, formulate a <code>.flatMap</code> such that each returned list is of a constant/pre-determined size. Whether or not that is useful for anything is another question.<br>
Before spoiling with the signature, understand that we need multiplication of naturals: If the original list has <strong>n</strong> elements, each of which is mapped to a list of <strong>m</strong> elements, the result after flattening shall have <strong>n*m</strong> elements.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"```\nsealed trait Natural {\n  type Plus[That \u003c: Natural] \u003c: Natural\n  type Mult[That \u003c: Natural] \u003c: Natural\n}\n```\nAgain the trivial case for Zero:  \n```\ncase object Zero extends Natural {\n  override type Plus[That \u003c: Natural] = That\n  override type Mult[That \u003c: Natural] = Zero.type\n}\n```\nand the more elaborate \n```\ncase class Suc[N \u003c: Natural]() extends Natural {\n  override type Plus[That \u003c: Natural] = Suc[N#Plus[That]]\n  override type Mult[That \u003c: Natural] = (N#Mult[That])#Plus[That]\n}\n```\nwhich leverages the distributive property of multiplication; note that at every iterative step one uses `(N + 1) x M = N x M + M` (where `N + 1` is to be interpreted as `Suc[N]`).  \nAgain, the formulation is recursive. It stops when on a step `N` corresponds to `Zero.type`."} -->
<div class="wp-block-jetpack-markdown"><pre><code>sealed trait Natural {
  type Plus[That &lt;: Natural] &lt;: Natural
  type Mult[That &lt;: Natural] &lt;: Natural
}
</code></pre>
<p>Again the trivial case for Zero:</p>
<pre><code>case object Zero extends Natural {
  override type Plus[That &lt;: Natural] = That
  override type Mult[That &lt;: Natural] = Zero.type
}
</code></pre>
<p>and the more elaborate</p>
<pre><code>case class Suc[N &lt;: Natural]() extends Natural {
  override type Plus[That &lt;: Natural] = Suc[N#Plus[That]]
  override type Mult[That &lt;: Natural] = (N#Mult[That])#Plus[That]
}
</code></pre>
<p>which leverages the distributive property of multiplication; note that at every iterative step one uses <code>(N + 1) x M = N x M + M</code> (where <code>N + 1</code> is to be interpreted as <code>Suc[N]</code>).<br>
Again, the formulation is recursive. It stops when on a step <code>N</code> corresponds to <code>Zero.type</code>.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"We are now capable of knowing at the type level the result of the multiplication of two natural numbers. This is essential and enables us to easily come up with a signature to `.flatMap`:  \n```\nsealed trait TypedList[+Element, Size \u003c: Natural] {\n  def flatMap[OtherElement, OtherSize](f: Element =\u003e TypedList[OtherElement, OtherSize]): TypedList[OtherElement, Size#Mult[OtherSize]\n}\n```\n```\ncase object TypedNil extends TypedList[Nothing, Zero.type]{\n  override def flatMap[OtherElement, OtherSize \u003c: Natural](f: Nothing =\u003e TypedList[OtherElement, OtherSize]): TypedList[OtherElement, Zero.type] = this\n}\n```\nBut it remains a challenge to come up with the actual implementation for the `TypedCons` case. After some thinking I came up with:  \n```\ncase class TypedCons[Element, Size \u003c: Natural](\n  override protected val _head: Element,\n  override protected val _tail: TypedList[Element, Size]\n) extends TypedList[Element, Suc[Size]] {\n\n override def concat[OtherElement \u003e: Element, OtherSize \u003c: Natural](that: TypedList[OtherElement, OtherSize]): TypedList[OtherElement, Suc[Size#Plus[OtherSize]]] =\n    TypedCons(head, tail.concat(that))\n\n override def flatMap[OtherElement, OtherSize \u003c: Natural](f: Element =\u003e TypedList[OtherElement, OtherSize]): TypedList[OtherElement, (Size#Mult[OtherSize])#Plus[OtherSize]] =  \n    f(head) concat tail.flatMap(f)\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><p>We are now capable of knowing at the type level the result of the multiplication of two natural numbers. This is essential and enables us to easily come up with a signature to <code>.flatMap</code>:</p>
<pre><code>sealed trait TypedList[+Element, Size &lt;: Natural] {
  def flatMap[OtherElement, OtherSize](f: Element =&gt; TypedList[OtherElement, OtherSize]): TypedList[OtherElement, Size#Mult[OtherSize]
}
</code></pre>
<pre><code>case object TypedNil extends TypedList[Nothing, Zero.type]{
  override def flatMap[OtherElement, OtherSize &lt;: Natural](f: Nothing =&gt; TypedList[OtherElement, OtherSize]): TypedList[OtherElement, Zero.type] = this
}
</code></pre>
<p>But it remains a challenge to come up with the actual implementation for the <code>TypedCons</code> case. After some thinking I came up with:</p>
<pre><code>case class TypedCons[Element, Size &lt;: Natural](
  override protected val _head: Element,
  override protected val _tail: TypedList[Element, Size]
) extends TypedList[Element, Suc[Size]] {

 override def concat[OtherElement &gt;: Element, OtherSize &lt;: Natural](that: TypedList[OtherElement, OtherSize]): TypedList[OtherElement, Suc[Size#Plus[OtherSize]]] =
    TypedCons(head, tail.concat(that))

 override def flatMap[OtherElement, OtherSize &lt;: Natural](f: Element =&gt; TypedList[OtherElement, OtherSize]): TypedList[OtherElement, (Size#Mult[OtherSize])#Plus[OtherSize]] =  
    f(head) concat tail.flatMap(f)
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"This recursive formulation is simple and elegant. It takes advantage of `.concat` and would do exactly what we want. Would! To my initial surprise, the compiler rejects it :\n```\n[error] type mismatch;\n[error]  found   : TypedList[OtherElement,OtherSize#Plus[Suc[Size]#Previous#Mult[OtherSize]]]\n[error]     (which expands to)  TypedList[OtherElement,OtherSize#Plus[Size#Mult[OtherSize]]]\n[error]  required: TypedList[OtherElement,Size#Mult[OtherSize]#Plus[OtherSize]]\n[error]     f(head) concat tail.flatMap(f)\n[error]             ^\n```"} -->
<div class="wp-block-jetpack-markdown"><p>This recursive formulation is simple and elegant. It takes advantage of <code>.concat</code> and would do exactly what we want. Would! To my initial surprise, the compiler rejects it :</p>
<pre><code>[error] type mismatch;
[error]  found   : TypedList[OtherElement,OtherSize#Plus[Suc[Size]#Previous#Mult[OtherSize]]]
[error]     (which expands to)  TypedList[OtherElement,OtherSize#Plus[Size#Mult[OtherSize]]]
[error]  required: TypedList[OtherElement,Size#Mult[OtherSize]#Plus[OtherSize]]
[error]     f(head) concat tail.flatMap(f)
[error]             ^
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"All this chained types might be a little to much for someone just scrolling down.  \nUsing more legible syntax, the compiler complains that it expects something of type `(Size x OtherSize) + OtherSize` but `f(head) concat tail.flatMap(f)` actually resolves to type `OtherSize + (Size x OtherSize)`.  \nSummation is commutative. Because we are sure we coded summation of Nats correctly, this too should end up, for any `Size` and `OtherSize`, to the same type. Well, it turns out the compiler doesn't know that !"} -->
<div class="wp-block-jetpack-markdown"><p>All this chained types might be a little to much for someone just scrolling down.<br>
Using more legible syntax, the compiler complains that it expects something of type <code>(Size x OtherSize) + OtherSize</code> but <code>f(head) concat tail.flatMap(f)</code> actually resolves to type <code>OtherSize + (Size x OtherSize)</code>.<br>
Summation is commutative. Because we are sure we coded summation of Nats correctly, this too should end up, for any <code>Size</code> and <code>OtherSize</code>, to the same type. Well, it turns out the compiler doesn't know that !</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"## I found two ways to solve this\n### 1. Acceptable trick  \nWhile `f(head) concat tail.flatMap(f)` above resolves to `OtherSize + (Size x OtherSize)`, if we revert the order to `tail.flatMap(f) concat f(head)` we get the type the compiler expects: `(Size x OtherSize) + OtherSize`.  \nBut `tail.flatMap(f) concat f(head)` does not do solely what we want; it also reverts the order of the elements on the list - as is apparent from the tail appearing first: \n```scala\nval foo: TypedList[Int, Nat3] = 1 :: 2 :: 3 :: TypedNil\ndef bar(i: Int): TypedList[Int, Nat2] = TypedCons(i*1, TypedCons(i*10))\nfoo.flatMap(bar)\n// res: TypedList[Int, Nat6] = TypedList(30, 30, 20, 20, 10, 10)\n```"} -->
<div class="wp-block-jetpack-markdown"><h2>I found two ways to solve this</h2>
<h3>1. Acceptable trick</h3>
<p>While <code>f(head) concat tail.flatMap(f)</code> above resolves to <code>OtherSize + (Size x OtherSize)</code>, if we revert the order to <code>tail.flatMap(f) concat f(head)</code> we get the type the compiler expects: <code>(Size x OtherSize) + OtherSize</code>.<br>
But <code>tail.flatMap(f) concat f(head)</code> does not do solely what we want; it also reverts the order of the elements on the list - as is apparent from the tail appearing first:</p>
<pre><code class="language-scala">val foo: TypedList[Int, Nat3] = 1 :: 2 :: 3 :: TypedNil
def bar(i: Int): TypedList[Int, Nat2] = TypedCons(i*1, TypedCons(i*10))
foo.flatMap(bar)
// res: TypedList[Int, Nat6] = TypedList(30, 30, 20, 20, 10, 10)
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"The trick, therefor, consists in reverting the list either before or after that `.flatMap` operation. Inefficient but effective. \n```scala\ncase class TypedCons[Element, Size \u003c: Natural](\n  override protected val _head: Element,\n  override protected val _tail: TypedList[Element, Size]\n) extends TypedList[Element, Suc[Size]] {\n\n  override def concat[OtherType \u003e: Element, OtherSize \u003c: Natural](that: TypedList[OtherType, OtherSize]): TypedList[OtherType, Suc[Size#Plus[OtherSize]]] =\n    TypedCons(head, tail.concat(that))\n\n  override def flatMap[OtherType, OtherSize \u003c: Natural](f: Element =\u003e TypedList[OtherType, OtherSize]): TypedList[OtherType, Size#Mult2[OtherSize]#Plus[OtherSize]] =\n    _flatMapInternal(f).reverse\n  override private [temp] def _flatMapInternal[OtherType, OtherSize \u003c: Natural](f: Element =\u003e TypedList[OtherType, OtherSize]): TypedList[OtherType, Size#Mult2[OtherSize]#Plus[OtherSize]] =\n    tail._flatMapInternal(f) concat f(head)\n\n  override def reverse: TypedList[Element, Suc[Size]] = tail.reverse :+ head\n\n  override def :+[A1 \u003e: Element](elem: A1): TypedCons[A1, Suc[Size]] = TypedCons(head, tail.:+(elem))\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><p>The trick, therefor, consists in reverting the list either before or after that <code>.flatMap</code> operation. Inefficient but effective.</p>
<pre><code class="language-scala">case class TypedCons[Element, Size &lt;: Natural](
  override protected val _head: Element,
  override protected val _tail: TypedList[Element, Size]
) extends TypedList[Element, Suc[Size]] {

  override def concat[OtherType &gt;: Element, OtherSize &lt;: Natural](that: TypedList[OtherType, OtherSize]): TypedList[OtherType, Suc[Size#Plus[OtherSize]]] =
    TypedCons(head, tail.concat(that))

  override def flatMap[OtherType, OtherSize &lt;: Natural](f: Element =&gt; TypedList[OtherType, OtherSize]): TypedList[OtherType, Size#Mult2[OtherSize]#Plus[OtherSize]] =
    _flatMapInternal(f).reverse
  override private [temp] def _flatMapInternal[OtherType, OtherSize &lt;: Natural](f: Element =&gt; TypedList[OtherType, OtherSize]): TypedList[OtherType, Size#Mult2[OtherSize]#Plus[OtherSize]] =
    tail._flatMapInternal(f) concat f(head)

  override def reverse: TypedList[Element, Suc[Size]] = tail.reverse :+ head

  override def :+[A1 &gt;: Element](elem: A1): TypedCons[A1, Suc[Size]] = TypedCons(head, tail.:+(elem))
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"Notice it was necessary to develop a `reverse` operation which in turn required the append operation `:+` as well.  \nMore importantly, the actual \u0022flatMapping\u0022 is done at `_flatMapInternal`. Being the responsibility of the api method `flatMap` just to call it and then reverse the result it gets. Now we obtain:  \n```scala\nval foo: TypedList = 1 :: 2 :: 3 :: TypedNil\ndef bar(i: Int): TypedList[Int, Nat2] = TypedCons(i*1, TypedCons(i*10))\nfoo.flatMap(bar)\n// res: TypedList[Int, Nat6] = TypedList(10, 10, 20, 20, 30, 30)\n```\n"} -->
<div class="wp-block-jetpack-markdown"><p>Notice it was necessary to develop a <code>reverse</code> operation which in turn required the append operation <code>:+</code> as well.<br>
More importantly, the actual &quot;flatMapping&quot; is done at <code>_flatMapInternal</code>. Being the responsibility of the api method <code>flatMap</code> just to call it and then reverse the result it gets. Now we obtain:</p>
<pre><code class="language-scala">val foo: TypedList = 1 :: 2 :: 3 :: TypedNil
def bar(i: Int): TypedList[Int, Nat2] = TypedCons(i*1, TypedCons(i*10))
foo.flatMap(bar)
// res: TypedList[Int, Nat6] = TypedList(10, 10, 20, 20, 30, 30)
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"### 2. Formulate multiplication at the type level differently\nThe formulation of multiplication above is correct. In fact it is the same as the one from these [guys on slide 27](#slick_scala), which do appear to know a lot about the issue.  \nTo solve our problem however, I just tried to change the order of the operands:  \n```scala\ncase class Suc[N]() extends Natural {\n  override type Mult[M \u003c: Natural] = M#Plus[N#Mult[M]]\n// instead of\n// override type Mult[M \u003c: Natural] = (N#Mult[M])#Plus[M]\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><h3>2. Formulate multiplication at the type level differently</h3>
<p>The formulation of multiplication above is correct. In fact it is the same as the one from these <a href="#slick_scala">guys on slide 27</a>, which do appear to know a lot about the issue.<br>
To solve our problem however, I just tried to change the order of the operands:</p>
<pre><code class="language-scala">case class Suc[N]() extends Natural {
  override type Mult[M &lt;: Natural] = M#Plus[N#Mult[M]]
// instead of
// override type Mult[M &lt;: Natural] = (N#Mult[M])#Plus[M]
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"The question is whether this formulation is equivalent. Does it also embody multiplication as required ? Yes, it does. All the below compile:  \n```scala\nimplicitly[Nat3#Mult[Nat2] =:= Nat6]\nimplicitly[Nat5#Mult[Nat2] =:= Nat10]\n\nimplicitly[Nat2#Mult[Nat9] =:= Nat2#Mult2[Nat9]]\nimplicitly[Nat3#Mult[Nat8] =:= Nat3#Mult2[Nat8]]\n```\nWhere `Mult2` corresponds to the initial \u0022encoding\u0022 of multiplication."} -->
<div class="wp-block-jetpack-markdown"><p>The question is whether this formulation is equivalent. Does it also embody multiplication as required ? Yes, it does. All the below compile:</p>
<pre><code class="language-scala">implicitly[Nat3#Mult[Nat2] =:= Nat6]
implicitly[Nat5#Mult[Nat2] =:= Nat10]

implicitly[Nat2#Mult[Nat9] =:= Nat2#Mult2[Nat9]]
implicitly[Nat3#Mult[Nat8] =:= Nat3#Mult2[Nat8]]
</code></pre>
<p>Where <code>Mult2</code> corresponds to the initial &quot;encoding&quot; of multiplication.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"With this new formulation for the type contructor field `Mult`, the compiller will glady accept\n```scala\ncase class TypedCons[Element, Size \u003c: Natural](\n  override protected val _head: Element,\n  override protected val _tail: TypedList[Element, Size]\n) extends TypedList[Element, Suc[Size]] {\n\n override def concat[OtherElement \u003e: Element, OtherSize \u003c: Natural](that: TypedList[OtherElement, OtherSize]): TypedList[OtherElement, Suc[Size#Plus[OtherSize]]] =\n    TypedCons(head, tail.concat(that))\n\n override def flatMap[OtherType, OtherSize \u003c: Natural](f: Element =\u003e TypedList[OtherType, OtherSize]): TypedList[OtherType, OtherSize#Plus[Size#Mult[OtherSize]]] =\n    f(head) concat tail.flatMap(f)\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><p>With this new formulation for the type contructor field <code>Mult</code>, the compiller will glady accept</p>
<pre><code class="language-scala">case class TypedCons[Element, Size &lt;: Natural](
  override protected val _head: Element,
  override protected val _tail: TypedList[Element, Size]
) extends TypedList[Element, Suc[Size]] {

 override def concat[OtherElement &gt;: Element, OtherSize &lt;: Natural](that: TypedList[OtherElement, OtherSize]): TypedList[OtherElement, Suc[Size#Plus[OtherSize]]] =
    TypedCons(head, tail.concat(that))

 override def flatMap[OtherType, OtherSize &lt;: Natural](f: Element =&gt; TypedList[OtherType, OtherSize]): TypedList[OtherType, OtherSize#Plus[Size#Mult[OtherSize]]] =
    f(head) concat tail.flatMap(f)
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"Without the need for that internal flatMap function and without the need for the reverse operation. This approach is not only more elegant, but also more efficient."} -->
<div class="wp-block-jetpack-markdown"><p>Without the need for that internal flatMap function and without the need for the reverse operation. This approach is not only more elegant, but also more efficient.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:heading {"level":3} -->
<h3>The .split</h3>
<!-- /wp:heading -->

<!-- wp:jetpack/markdown {"source":"At last the `.split` operation. The most difficult to implement.  \nWe are interested in the following behaviour:  \n```scala\nval foo: TypedList[String, Nat5] = \u0022Foo\u0022 :: \u0022Bar\u0022 :: \u0022Baz\u0022 :: \u0022Qux\u0022 :: \u0022Quux\u0022 :: TypedNil\n\nval (left, right) = foo.split[Nat2]\n// res (left): TypedList[String, Nat2] = TypedList(Foo, Bar)\n// res (right): TypedList[String, Nat3] = TypedList(Baz, Qux, Quux)\n```"} -->
<div class="wp-block-jetpack-markdown"><p>At last the <code>.split</code> operation. The most difficult to implement.<br>
We are interested in the following behaviour:</p>
<pre><code class="language-scala">val foo: TypedList[String, Nat5] = &quot;Foo&quot; :: &quot;Bar&quot; :: &quot;Baz&quot; :: &quot;Qux&quot; :: &quot;Quux&quot; :: TypedNil

val (left, right) = foo.split[Nat2]
// res (left): TypedList[String, Nat2] = TypedList(Foo, Bar)
// res (right): TypedList[String, Nat3] = TypedList(Baz, Qux, Quux)
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"If we want the resulting lists to be typed, then we need to encode subtraction of natural numbers. Above, the compiler needed to know how to subtract 2 from 5.  "} -->
<div class="wp-block-jetpack-markdown"><p>If we want the resulting lists to be typed, then we need to encode subtraction of natural numbers. Above, the compiler needed to know how to subtract 2 from 5.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"Encoding multiplication is easy once you read how to encode summation.  \nSubtraction was trickier. With the help of @Alexey Romanov in these [SO question](https://stackoverflow.com/questions/53802117/scala-subtracting-two-natural-numbers-at-the-type-level), we have:   \n"} -->
<div class="wp-block-jetpack-markdown"><p>Encoding multiplication is easy once you read how to encode summation.<br>
Subtraction was trickier. With the help of @Alexey Romanov in these <a href="https://stackoverflow.com/questions/53802117/scala-subtracting-two-natural-numbers-at-the-type-level">SO question</a>, we have:</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"\n```scala\npackage foo\nsealed trait Natural {\n  type Previous \u003c: Natural\n  private [foo] type _Minus[M \u003c: Natural] \u003c: Natural\n  type Minus[M \u003c: Natural] = M#_Minus[Suc[Previous]]\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><pre><code class="language-scala">package foo
sealed trait Natural {
  type Previous &lt;: Natural
  private [foo] type _Minus[M &lt;: Natural] &lt;: Natural
  type Minus[M &lt;: Natural] = M#_Minus[Suc[Previous]]
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"This encoding might feel confusing. Reading the SO thread might shed some light.  \nThis hack is very much the same as the \u00221st solution\u0022 for the flatMap problem discussed previously, where we had an internal `_flatMap`. This is the type-level counterpart.  \nThe type constructor to be exposed is `Minus[M \u003c: Natural]`. `_Minus` is internal and a way to achieve the semantics of subtraction. "} -->
<div class="wp-block-jetpack-markdown"><p>This encoding might feel confusing. Reading the SO thread might shed some light.<br>
This hack is very much the same as the &quot;1st solution&quot; for the flatMap problem discussed previously, where we had an internal <code>_flatMap</code>. This is the type-level counterpart.<br>
The type constructor to be exposed is <code>Minus[M &lt;: Natural]</code>. <code>_Minus</code> is internal and a way to achieve the semantics of subtraction.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"```scala\ncase object Zero extends Natural {\n  override type Previous = Zero.type\n  override private [foo] type _Minus[M \u003c: Natural] = M\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><pre><code class="language-scala">case object Zero extends Natural {
  override type Previous = Zero.type
  override private [foo] type _Minus[M &lt;: Natural] = M
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"```scala\ncase class Suc[N \u003c: Natural]() extends Natural {\n  override type Previous = N\n  override private [foo] type _Minus[M \u003c: Natural] = Previous#_Minus[M#Previous]\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><pre><code class="language-scala">case class Suc[N &lt;: Natural]() extends Natural {
  override type Previous = N
  override private [foo] type _Minus[M &lt;: Natural] = Previous#_Minus[M#Previous]
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"Notice that `_Minus` above, on its own, would only work if `M` was greater than `Suc[N]`:\n```scala\nNat2#_Minus[Nat3] = \nNat1#_Minus[Nat2] = \nNat0#_Minus[Nat1] = Nat1\n// Good\n```\nBut \n```scala\nNat3#_Minus[Nat2] = \nNat2#_Minus[Nat1] = \nNat1#_Minus[Nat0] = \nNat0#_Minus[Nat0] = Nat0\n// Bad\n```\nTherefore, we hide `_Minus` and wrap it within `Minus`, which, before calling `_Minus` inverts the operands (swap the the n and m in `n - m`)"} -->
<div class="wp-block-jetpack-markdown"><p>Notice that <code>_Minus</code> above, on its own, would only work if <code>M</code> was greater than <code>Suc[N]</code>:</p>
<pre><code class="language-scala">Nat2#_Minus[Nat3] = 
Nat1#_Minus[Nat2] = 
Nat0#_Minus[Nat1] = Nat1
// Good
</code></pre>
<p>But</p>
<pre><code class="language-scala">Nat3#_Minus[Nat2] = 
Nat2#_Minus[Nat1] = 
Nat1#_Minus[Nat0] = 
Nat0#_Minus[Nat0] = Nat0
// Bad
</code></pre>
<p>Therefore, we hide <code>_Minus</code> and wrap it within <code>Minus</code>, which, before calling <code>_Minus</code> inverts the operands (swap the the n and m in <code>n - m</code>)</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"```\nWe then obtain the desired behaviour:\n// Subtracting n-m:\n\n// Works when n\u003em\nimplicitly[Nat9#Minus[Nat5] =:= Nat4]   // compiles\n\n// Gives 0 when n\u003cm (more than fine. We are not interested nor model the negative numbers)\nimplicitly[Nat0#Minus[Nat5] =:= Nat0]   // compiles\nimplicitly[Nat2#Minus[Nat5] =:= Nat0]   // compiles\n```"} -->
<div class="wp-block-jetpack-markdown"><pre><code>We then obtain the desired behaviour:
// Subtracting n-m:

// Works when n&gt;m
implicitly[Nat9#Minus[Nat5] =:= Nat4]   // compiles

// Gives 0 when n&lt;m (more than fine. We are not interested nor model the negative numbers)
implicitly[Nat0#Minus[Nat5] =:= Nat0]   // compiles
implicitly[Nat2#Minus[Nat5] =:= Nat0]   // compiles
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"We have defined the types to be returned by the `split` method.  Proceeding to defining the methods themselves.  \n```scala\nsealed trait TypedList[+Element, Size \u003c: Natural] {\n  def split[At \u003c: Suc[_ \u003c: Natural]]: (TypedList[Element, At], TypedList[Element, Size#Minus[At])\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><p>We have defined the types to be returned by the <code>split</code> method.  Proceeding to defining the methods themselves.</p>
<pre><code class="language-scala">sealed trait TypedList[+Element, Size &lt;: Natural] {
  def split[At &lt;: Suc[_ &lt;: Natural]]: (TypedList[Element, At], TypedList[Element, Size#Minus[At])
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"For `TypedNil` case we will just thrown an exception because we will be protecting, further down, the method from access.  \nFor `TypedCons`, my idea was:  \nTo split `TypedList(\u0022Foo\u0022, \u0022Bar\u0022, \u0022Baz\u0022, \u0022Qux\u0022, \u0022Quux\u0022)` at index `Nat3`:  \n1. Construct a `TypedList[Natural, Nat3](nat1, nat2, nat3)`.  \n2. Construct a `TypedList[Natural, Nat2](nat4, nat5)`. \n3. Develop a `get(index: Natural)` method, on our `TypedList`, such that it will retrieve the element of the list at said index.  \n4. Map lists 1. and 2. passing them function `get`. "} -->
<div class="wp-block-jetpack-markdown"><p>For <code>TypedNil</code> case we will just thrown an exception because we will be protecting, further down, the method from access.<br>
For <code>TypedCons</code>, my idea was:<br>
To split <code>TypedList(&quot;Foo&quot;, &quot;Bar&quot;, &quot;Baz&quot;, &quot;Qux&quot;, &quot;Quux&quot;)</code> at index <code>Nat3</code>:</p>
<ol>
<li>Construct a <code>TypedList[Natural, Nat3](nat1, nat2, nat3)</code>.</li>
<li>Construct a <code>TypedList[Natural, Nat2](nat4, nat5)</code>.</li>
<li>Develop a <code>get(index: Natural)</code> method, on our <code>TypedList</code>, such that it will retrieve the element of the list at said index.</li>
<li>Map lists 1. and 2. passing them function <code>get</code>.</li>
</ol>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"```scala\ncase class TypedCons[Element, Size \u003c: Natural](\n  override protected val _head: Element,\n  override protected val _tail: TypedList[Element, Size]\n) extends TypedList[Element, Suc[Size]] {\n  override def split[At \u003c: Suc[_ \u003c: Natural]](\n    implicit n: At,\n    leftList: TypedList[Natural, At],\n    rightList: TypedList[Natural, Suc[Size]#Minus[At]]\n): (TypedList[Element, At], TypedList[Element, Suc[Size]#Minus[At]]) = \n  (\n    leftList.map(_get),\n    rightList.map(i =\u003e _get(i.plus(n)))\n  )\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><pre><code class="language-scala">case class TypedCons[Element, Size &lt;: Natural](
  override protected val _head: Element,
  override protected val _tail: TypedList[Element, Size]
) extends TypedList[Element, Suc[Size]] {
  override def split[At &lt;: Suc[_ &lt;: Natural]](
    implicit n: At,
    leftList: TypedList[Natural, At],
    rightList: TypedList[Natural, Suc[Size]#Minus[At]]
): (TypedList[Element, At], TypedList[Element, Suc[Size]#Minus[At]]) = 
  (
    leftList.map(_get),
    rightList.map(i =&gt; _get(i.plus(n)))
  )
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"Requiring 3 implicits, this signature seems daunting.  \nWhy are `leftList` and `rightList` relevant ?  \n1. Because they provide typed lists of the the same size of the ones the function has to return! - half the job is done right here. \n2. Because their elements are the sequence of natural numbers starting at nat1.  \nAnd why is this relevant?\n    * because we also have a function `_get` that retrieves an element from a typed list if given an index!\n"} -->
<div class="wp-block-jetpack-markdown"><p>Requiring 3 implicits, this signature seems daunting.<br>
Why are <code>leftList</code> and <code>rightList</code> relevant ?</p>
<ol>
<li>Because they provide typed lists of the the same size of the ones the function has to return! - half the job is done right here.</li>
<li>Because their elements are the sequence of natural numbers starting at nat1.<br>
And why is this relevant?
<ul>
<li>because we also have a function <code>_get</code> that retrieves an element from a typed list if given an index!</li>
</ul>
</li>
</ol>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"Where we don't show the implementation of `_get` - which is not difficult to formulate.  \nBut how is the compiler able to find these implicits? Because we defined, on the implicit search scope, and for these function only, specificly \u0022engineered\u0022 implicits:  \n```scala\nimplicit def typedListOfNats[N \u003c: Natural](implicit previousNatTypedList: TypedList[Natural, N], thisNat: Suc[N]): TypedList[Natural, Suc[N]] =\n    previousNatTypedList :+ thisNat\n\nimplicit def emptyList[A]: TypedList[A, Zero.type] = TypedNil\n```"} -->
<div class="wp-block-jetpack-markdown"><p>Where we don't show the implementation of <code>_get</code> - which is not difficult to formulate.<br>
But how is the compiler able to find these implicits? Because we defined, on the implicit search scope, and for these function only, specificly &quot;engineered&quot; implicits:</p>
<pre><code class="language-scala">implicit def typedListOfNats[N &lt;: Natural](implicit previousNatTypedList: TypedList[Natural, N], thisNat: Suc[N]): TypedList[Natural, Suc[N]] =
    previousNatTypedList :+ thisNat

implicit def emptyList[A]: TypedList[A, Zero.type] = TypedNil
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"It is hopefully not difficult to understand the above implicits allow for the behaviour:  \n```scala\nimplicitly[TypedList[Nat4]]\n//res: TypedList[Natural, Nat4] = TypedList(nat1, nat2, nat3, nat4)"} -->
<div class="wp-block-jetpack-markdown"><p>It is hopefully not difficult to understand the above implicits allow for the behaviour:</p>
<pre><code class="language-scala">implicitly[TypedList[Nat4]]
//res: TypedList[Natural, Nat4] = TypedList(nat1, nat2, nat3, nat4)</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"Lastly, notice that we are still able to split a TypedList by an index greater than the list's size:  \n```scala\nval foo: TypedList[String, Nat5] = \u0022Foo\u0022 :: \u0022Bar\u0022 :: \u0022Baz\u0022 :: \u0022Qux\u0022 :: \u0022Quux\u0022 :: TypedNil\n\nfoo.split[Nat6]\n// res: java.lang.Exception: Boom!\n``` "} -->
<div class="wp-block-jetpack-markdown"><p>Lastly, notice that we are still able to split a TypedList by an index greater than the list's size:</p>
<pre><code class="language-scala">val foo: TypedList[String, Nat5] = &quot;Foo&quot; :: &quot;Bar&quot; :: &quot;Baz&quot; :: &quot;Qux&quot; :: &quot;Quux&quot; :: TypedNil

foo.split[Nat6]
// res: java.lang.Exception: Boom!
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"Restraining calls to method `split` for indexes greater than the list's size is more demanding than using a phatom type as done for protecting calls to `head` and `tail` on an empty list. Instead, we need to protect this calls with an implicit evidence. \n```scala\ndef split[At \u003c: Suc[_ \u003c: Natural]](implicit ev: LowerOrEqual[At, Size])\n```"} -->
<div class="wp-block-jetpack-markdown"><p>Restraining calls to method <code>split</code> for indexes greater than the list's size is more demanding than using a phatom type as done for protecting calls to <code>head</code> and <code>tail</code> on an empty list. Instead, we need to protect this calls with an implicit evidence.</p>
<pre><code class="language-scala">def split[At &lt;: Suc[_ &lt;: Natural]](implicit ev: LowerOrEqual[At, Size])
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"The rationale goes as follows: \nEvery time `split[At]` (from within given `TypedList[Element, Size]`) is called, the compiler must be able to find an instance for the concrete type `LowerOrEqual[At, Size]`, where `At` is the concrete type with which `split` was called and `Size` is the concrete type of the list. If it is not able, it will fail with a compilation error.  \nOur job is therefore to come up with `LowerOrEqual[N \u003c: Natural, M \u003c: Natural]` and corresponding implicits such that they (the implicits), can only be found if `N \u003c= M`."} -->
<div class="wp-block-jetpack-markdown"><p>The rationale goes as follows:
Every time <code>split[At]</code> (from within given <code>TypedList[Element, Size]</code>) is called, the compiler must be able to find an instance for the concrete type <code>LowerOrEqual[At, Size]</code>, where <code>At</code> is the concrete type with which <code>split</code> was called and <code>Size</code> is the concrete type of the list. If it is not able, it will fail with a compilation error.<br>
Our job is therefore to come up with <code>LowerOrEqual[N &lt;: Natural, M &lt;: Natural]</code> and corresponding implicits such that they (the implicits), can only be found if <code>N &lt;= M</code>.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"```\nimport scala.annotation.implicitNotFound\n\n@implicitNotFound(\u0022${N} is not lower or equal to ${M}\u0022)\nsealed trait LowerOrEqual[N \u003c: Natural, M \u003c: Natural]\n\nobject LowerOrEqual {\n  implicit def foo[N \u003c: Natural, M \u003c: Natural](implicit ev: LowerOrEqual[N, M]): LowerOrEqual[Suc[N], Suc[M]] = new LowerOrEqual[Suc[N], Suc[M]] {}\n  implicit def bar[M \u003c: Natural]: LowerOrEqual[Zero.type, M] = new LowerOrEqual[Zero.type, M] {}\n}\n```"} -->
<div class="wp-block-jetpack-markdown"><pre><code>import scala.annotation.implicitNotFound

@implicitNotFound(&quot;${N} is not lower or equal to ${M}&quot;)
sealed trait LowerOrEqual[N &lt;: Natural, M &lt;: Natural]

object LowerOrEqual {
  implicit def foo[N &lt;: Natural, M &lt;: Natural](implicit ev: LowerOrEqual[N, M]): LowerOrEqual[Suc[N], Suc[M]] = new LowerOrEqual[Suc[N], Suc[M]] {}
  implicit def bar[M &lt;: Natural]: LowerOrEqual[Zero.type, M] = new LowerOrEqual[Zero.type, M] {}
}
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"Notice that: \n1. trait `LowerOrEqual` doesn't need to have any fields (vals, vars, defs, ....) . It is the type itself (in this case a type constructor), represented by `LowerOrEqual`, and the implicit defs that matter for what we want to achieve.\n2. The names `foo` and `bar` are also pretty irrelevant."} -->
<div class="wp-block-jetpack-markdown"><p>Notice that:</p>
<ol>
<li>trait <code>LowerOrEqual</code> doesn't need to have any fields (vals, vars, defs, ....) . It is the type itself (in this case a type constructor), represented by <code>LowerOrEqual</code>, and the implicit defs that matter for what we want to achieve.</li>
<li>The names <code>foo</code> and <code>bar</code> are also pretty irrelevant.</li>
</ol>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"We can \u0022unit test\u0022 that the above encapsulates the constraint we want:\n"} -->
<div class="wp-block-jetpack-markdown"><p>We can &quot;unit test&quot; that the above encapsulates the constraint we want:</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"```\nimplicitly[LowerOrEqual[Nat5, Nat6]]    // compiles (i.e. implicit found)\nimplicitly[LowerOrEqual[Nat2, Nat2]]    // compiles (i.e. implicit found)\n\nimplicitly[LowerOrEqual[Nat6, Nat5]]    // does not compile (i.e. implicit not found)\n// error: Nat6 is not lower or equal to Nat5\n```"} -->
<div class="wp-block-jetpack-markdown"><pre><code>implicitly[LowerOrEqual[Nat5, Nat6]]    // compiles (i.e. implicit found)
implicitly[LowerOrEqual[Nat2, Nat2]]    // compiles (i.e. implicit found)

implicitly[LowerOrEqual[Nat6, Nat5]]    // does not compile (i.e. implicit not found)
// error: Nat6 is not lower or equal to Nat5
</code></pre>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"How exactly do those two `foo` and `bar` implicit defs achieve this behavior ? Via implicit recursion and clever engineering of the return types of the two functions. Lets study the possible cases:"} -->
<div class="wp-block-jetpack-markdown"><p>How exactly do those two <code>foo</code> and <code>bar</code> implicit defs achieve this behavior ? Via implicit recursion and clever engineering of the return types of the two functions. Lets study the possible cases:</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"\n**case 1**. `LowerOrEqual[Nat0, NatY]` (for some `NatY`)     \nThe compiler realizes function `bar` can return the desired type (for the appropriate `M` in `bar[M \u003c: Natural]`. Problem solved.  \n**case 2**. `LowerOrEqual[NatX, Nat0]`  (for some `NatX` not `Nat0`)  \nThe compiler realizes neither function `bar` or `foo` may ever return the necessary type.  Compiler error.  \n**case 3**. `LowerOrEqual[NatX, NatY]` (where `NatX` and `NatY` are not `Nat0`)  \nThe compiler understands function `foo` may potentially return the desired type.  For that however, it needs to find yet another implicit: the argument `ev` in `foo`.  And so this gives origin to a recursive implicit resolution by the compiler, which ultimately leads to either **case 1**, thus compiling, or **case 2**, thus not compiling.  \nYou should be able to notice, by looking at the return type of function `foo` and the type of `ev` that:  \na. **Case 1** is reached if and only if `N \u003c M`    \nb. **Case 2** is reached if and only if `N \u003e M`  \n\n  "} -->
<div class="wp-block-jetpack-markdown"><p><strong>case 1</strong>. <code>LowerOrEqual[Nat0, NatY]</code> (for some <code>NatY</code>)<br>
The compiler realizes function <code>bar</code> can return the desired type (for the appropriate <code>M</code> in <code>bar[M &lt;: Natural]</code>. Problem solved.<br>
<strong>case 2</strong>. <code>LowerOrEqual[NatX, Nat0]</code>  (for some <code>NatX</code> not <code>Nat0</code>)<br>
The compiler realizes neither function <code>bar</code> or <code>foo</code> may ever return the necessary type.  Compiler error.<br>
<strong>case 3</strong>. <code>LowerOrEqual[NatX, NatY]</code> (where <code>NatX</code> and <code>NatY</code> are not <code>Nat0</code>)<br>
The compiler understands function <code>foo</code> may potentially return the desired type.  For that however, it needs to find yet another implicit: the argument <code>ev</code> in <code>foo</code>.  And so this gives origin to a recursive implicit resolution by the compiler, which ultimately leads to either <strong>case 1</strong>, thus compiling, or <strong>case 2</strong>, thus not compiling.<br>
You should be able to notice, by looking at the return type of function <code>foo</code> and the type of <code>ev</code> that:<br>
a. <strong>Case 1</strong> is reached if and only if <code>N &lt; M</code><br>
b. <strong>Case 2</strong> is reached if and only if <code>N &gt; M</code></p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:jetpack/markdown {"source":"The annotation `implicitNotFound`'s purpose is for more informative errors for clients of library. We would get otherwise: `could not find implicit value for parameter e: LowerOrEqual[Nat6,Nat5]`. Which actually sheds more light on the internals of what is going on.  "} -->
<div class="wp-block-jetpack-markdown"><p>The annotation <code>implicitNotFound</code>'s purpose is for more informative errors for clients of library. We would get otherwise: <code>could not find implicit value for parameter e: LowerOrEqual[Nat6,Nat5]</code>. Which actually sheds more light on the internals of what is going on.</p>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:heading {"level":3} -->
<h3>Footnotes</h3>
<!-- /wp:heading -->

<!-- wp:jetpack/markdown {"source":"1. Here I extend the behavior of `zip` to applying any given function to a pair of elements, instead of just joining the two elements in a tuple.\n2. The motivation for developing this typed list and blog post was a very nice talk by [Joe Barnes](#joe_barnes_talk).   \nAnother essential source knowledge was this blog post from [Yao Li](#yao_li), which I highly recommend. That said, I have derived everything on the lib on my own and added several new features, most important of these being the `split` operation and type constructor `Minus` on naturals.  "} -->
<div class="wp-block-jetpack-markdown"><ol>
<li>Here I extend the behavior of <code>zip</code> to applying any given function to a pair of elements, instead of just joining the two elements in a tuple.</li>
<li>The motivation for developing this typed list and blog post was a very nice talk by <a href="#joe_barnes_talk">Joe Barnes</a>.<br>
Another essential source knowledge was this blog post from <a href="#yao_li">Yao Li</a>, which I highly recommend. That said, I have derived everything on the lib on my own and added several new features, most important of these being the <code>split</code> operation and type constructor <code>Minus</code> on naturals.</li>
</ol>
</div>
<!-- /wp:jetpack/markdown -->

<!-- wp:heading {"level":3} -->
<h3>References</h3>
<!-- /wp:heading -->

<!-- wp:html -->
<p> 1. <a name="joe_barnes_talk" class="mce-item-anchor"></a> Barnes, Joe. Typelevel Programming 101: The Subspace of Scala. <br>
https://www.youtube.com/watch?v=_-J4YRI1rAw&amp;t=5s </p>

<p> 2. <a name="yao_li" class="mce-item-anchor"></a> Li, Yao. Dependent Types in Scala. <br>  
https://lastland.github.io/pl-blog/posts/2017-09-03-dtscala.html </p>

<p> 3. <a name="slick_scala" class="mce-item-anchor"></a> Zeiger, Stefan. Type-Level Computations in Scala. <br>
http://slick.lightbend.com/talks/scalaio2014/Type-Level_Computations.pdf </p>
<!-- /wp:html -->

<!-- wp:paragraph -->
<p> </p>
<!-- /wp:paragraph -->