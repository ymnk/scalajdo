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

import javax.jdo._
import _root_.scala.collection.jcl.Conversions.convertList
import _root_.scala.collection.mutable.Set
import _root_.java.util.Arrays
import _root_.java.util.{List => JList}

class ScalaQuery[A](val pm:PersistenceManager, val clss:Class[A]){

  private val filterCriteria = Set.empty[FilterCriterion]
  private val orderCriteria = Set.empty[OrderCriterion]
  private var rangeIndex:Option[(Long, Long)] = None
  private var _timeoutMillis:Option[Int] = None

  def findAll = getResultList()

  def getResultList() = {
    val q = newQuery
    try{
      convertList[A](q.executeWithArray(parameters:_*).asInstanceOf[JList[A]])
    }
    finally{
      q.closeAll
    }
  }

  def findOne():Option[A] = toOption(getSingleResult)

  private def toOption[A](a:A):Option[A] = a match{
    case null => None
    case a => Some(a)
  }

  def getSingleResult() = {
    val query = newQuery
    try{
      query.setUnique(true)
      query.executeWithArray(parameters:_*).asInstanceOf[A]
    }
    finally{
      query.closeAll
    }
  }

  def getFirstResult():Option[A] ={
    range(0, 1)
    getResultList match{
      case l if l.size==0 => None
      case l => Some(l(0))
    }
  }

  def newQuery={
    val query = pm.newQuery(clss)
    filter match{
      case "" =>
      case f => query.setFilter(f)
    }
    ordering match{
      case "" =>
      case o => query.setOrdering(o)
    }
    rangeIndex match{
      case Some((s, e)) => query.setRange(s, e)
      case _ =>
    }
    _timeoutMillis match{
      case None =>
      case Some(t) => query.setTimeoutMillis(t)
    }
    query
  }

  def where(criteria:FilterCriterion *) = {
    filterCriteria ++ criteria
    this
  }

  def orderBy(criteria:OrderCriterion *) = {
    orderCriteria ++ criteria
    this
  }

  def range(startIndex:Long, endIndex:Long) = {
    rangeIndex = Some((startIndex, endIndex))
    this
  }

  def timeoutMillis(timeoutMillis:Int) = {
    _timeoutMillis = Some(timeoutMillis)
    this
  }

  private def parameters() ={
    filterCriteria.map(_.parameter).toList.toArray
  } 

  private def filter()={
    filterCriteria.toList.zipWithIndex.map{
      case (f, i) => f.queryString(":"+i)
    }.mkString(" && ")
  }

  private def ordering()={
    orderCriteria.toList.map(_.queryString()).mkString(", ")
  }

  def queryStringWithParameters()={
    queryString + " with " + Arrays.asList(parameters:_*)
  }

  def queryString()={
    val sb = new StringBuilder
    sb.append("select from ").append(clss.getName)
    filter match{
      case "" =>
      case f => sb.append(" where ").append(f)	
    }
    ordering match{
      case "" =>
      case o => sb.append(" order by ").append(o)
    }
    rangeIndex match{
      case Some((s, e)) =>
        sb.append(" range ").append(s).append(", ").append(e)
      case _ =>
    }
    sb.toString
  }
}
 
 
 
