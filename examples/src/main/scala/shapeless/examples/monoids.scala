/*
 * Copyright (c) 2012-3 Miles Sabin 
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package shapeless.examples

import shapeless._

object MonoidExamples extends App {
  import MonoidSyntax._
  import GenericMonoid._ // One line of boilerplate at use sites ... see further comment below.

  // A pair of arbitrary case classes
  case class Foo(i : Int, s : String)
  case class Bar(b : Boolean, s : String, d : Double)

  // Automatically they're monoids ...
  implicitly[Monoid[Foo]]
  val f = Foo(13, "foo") |+| Foo(23, "bar")
  assert(f == Foo(36, "foobar"))

  implicitly[Monoid[Bar]]
  val b = Bar(true, "foo", 1.0) |+| Bar(false, "bar", 3.0)
  assert(b == Bar(true, "foobar", 4.0))
}

/**
 * Pedagogic subset of the Scalaz monoid
 */
trait Monoid[T] {
  def zero : T
  def append(a : T, b : T) : T
}

object Monoid {
  def mzero[T](implicit mt : Monoid[T]) = mt.zero
  
  implicit def booleanMonoid : Monoid[Boolean] = new Monoid[Boolean] {
    def zero = false
    def append(a : Boolean, b : Boolean) = a || b
  }
  
  implicit def intMonoid : Monoid[Int] = new Monoid[Int] {
    def zero = 0
    def append(a : Int, b : Int) = a+b
  }
  
  implicit def doubleMonoid : Monoid[Double] = new Monoid[Double] {
    def zero = 0.0
    def append(a : Double, b : Double) = a+b
  }
  
  implicit def stringMonoid : Monoid[String] = new Monoid[String] {
    def zero = ""
    def append(a : String, b : String) = a+b
  }
}

trait MonoidSyntax[T] {
  def |+|(b : T) : T
}

object MonoidSyntax {
  implicit def monoidSyntax[T](a : T)(implicit mt : Monoid[T]) : MonoidSyntax[T] = new MonoidSyntax[T] {
    def |+|(b : T) = mt.append(a, b)
  }
}

// Note that this only needs to be separate from the primary type class companion object 
// (ie. Monoid above) if you don't want any form of dependency shapeless. If a shapeless
// dependency isn't problematic then you should have your type class companion extend
// TypeClass directly. In this case the generic implicits will be included in your type
// classes implicit scope automatically, and the import at use sites (ie. the
// "import GenericMonoid._" above) will no longer be necessary

object GenericMonoid extends TypeClass[Monoid] {
  def emptyProduct = new Monoid[HNil] {
    def zero = HNil
    def append(a : HNil, b : HNil) = HNil
  }
  
  def product[F, T <: HList](FHead : Monoid[F], FTail : Monoid[T]) = new Monoid[F :: T] {
    def zero = FHead.zero :: FTail.zero
    def append(a : F :: T, b : F :: T) = FHead.append(a.head, b.head) :: FTail.append(a.tail, b.tail)
  }
  
  def derive[F, Repr](instance : Monoid[Repr], gen : GenericAux[F, Repr]) = new Monoid[F] {
    def zero = gen.from(instance.zero)
    def append(a : F, b : F) = gen.from(instance.append(gen.to(a), gen.to(b)))
  }
}

// vim: expandtab:ts=2:sw=2
