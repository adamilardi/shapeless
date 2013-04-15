/*
 * Copyright (c) 2011 Miles Sabin 
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

package shapeless

import scala.language.existentials
  
import org.junit.Test
import org.junit.Assert._

import test.illTyped
import Nat._

class HKernelTests {
  def typed[T](t : => T) {}
  
  type K = Int :: String :: Boolean :: HNil
  type L = List[Int] :: List[String] :: List[Boolean] :: HNil
  type O = Option[Int] :: Option[String] :: Option[Boolean] :: HNil
  
  type HK = HConsHKernel[Int, HConsHKernel[String, HConsHKernel[Boolean, HNilHKernel]]]
  
  val l1 = List(23) :: List("foo") :: List(true) :: HNil
  val l2 = 23 :: "foo" :: true :: HNil
  
  val hka1 = implicitly[HKernelAux[L]]
  
  abstract class HKernelOps[L <: HList, HK <: HKernel](l: L, val kernel: HK) {
    type F[_]
    implicit val ev: L =:= kernel.Mapped[F] 
    
    def map[G[_]](f: F ~> G) = kernel.map(f, l)
    
    def length = kernel.length
  }

  implicit def mkHKernel[L <: HList](l: L)(implicit hka: HKernelAux[L]) = new HKernelOps[L, hka.Out](l, hka()) {
    type F[X] = hka.F[X]
    implicit val ev: L =:= kernel.Mapped[F] = ???
  }
  
  mkHKernel(l1)
  mkHKernel(l2)
  
  //val l3a = l1.map(headOption)
  val l3 = l1.map(headOption)
  typed[O](l3)
  val sz = l1.length
  typed[_3](sz)
  
  trait Outer[F0[_]] {
    type λ[L <: HList] = HKernelAux[L] { type F[X] = F0[X] }
  }
  
  def blah[L <: HList : Outer[List]#λ](l : L) = {
//    illTyped("""
//      l.map(headOption)
//    """)
    l.map(headOption)
  }
  
  def len[L <: HList, HK <: HKernelAux[L]](l : L)(implicit kernel : HK) = kernel().length
}