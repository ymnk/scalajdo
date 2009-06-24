/*
 * Copyright 2009 ymnk, JCraft,Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package org.scala_libs.jdo

trait Criterion{
  val property:String
}
trait FilterCriterion extends Criterion{
  val parameter:AnyRef
  def queryString(paramerterName:String):String
}

class ContainsCriterion(override val property:String, 
			override val parameter:AnyRef) extends FilterCriterion{
  def queryString(parameterName:String) =
    "%s.contains(%s)".format(property, parameterName)
}
class EqCriterion(override val property:String, 
		  override val parameter:AnyRef) extends FilterCriterion{
  def queryString(parameterName:String) =
    "%s == %s".format(property, parameterName)
}
class GeCriterion(override val property:String, 
		  override val parameter:AnyRef) extends FilterCriterion{
  def queryString(parameterName:String) =
    "%s >= %s".format(property, parameterName)
}
class GtCriterion(override val property:String, 
		  override val parameter:AnyRef) extends FilterCriterion{
  def queryString(parameterName:String) =
    "%s > %s".format(property, parameterName)
}
class LeCriterion(override val property:String, 
		  override val parameter:AnyRef) extends FilterCriterion{
  def queryString(parameterName:String) =
    "%s <= %s".format(property, parameterName)
}
class LtCriterion(override val property:String, 
		  override val parameter:AnyRef) extends FilterCriterion{
  def queryString(parameterName:String) =
    "%s < %s".format(property, parameterName)
}


trait OrderCriterion extends Criterion{
  def queryString():String
}
class AscCriterion(override val property:String) extends OrderCriterion{
  def queryString() = property+" asc"
}
class DescCriterion(override val property:String) extends OrderCriterion{
  def queryString() = property+" desc"
}
