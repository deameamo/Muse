package com.deameamo.commons.util

import scala.collection.mutable

class ArrayList[A] extends mutable.MutableList[A] {

  def replaceElem(original: A, replacement: A): this.type = {
    val index = indexOf(original)
    val temp = (take(index) += replacement) ++ drop(index + 1)
    clear
    this ++= temp
    this
  }
  
  def -=(a: A): this.type = { removeElement(a) }
  
  def removeElements(list: mutable.MutableList[A]) {
    list.foreach(removeElement)
  }
  
  def removeElement(a: A): this.type = { removeAtIndex(indexOf(a)) }
  
  def removeAtIndex(index: Int): this.type = {
    if(index >= 0 && index < size) {
      val temp = take(index) ++ drop(index + 1)
      clear
      this ++= temp
    }
    this
  }
  
  def insertBefore(index: Int, a: A): this.type = {
    if(size == 0)
      this += a
    else {
      if(index >= 0 && index < size) {
        val temp = (take(index) += a) ++ drop(index)
        clear
        this ++= temp
      }
    }
    this
  }
  
  def insertAfter(index: Int, a: A): this.type = {
    if(size == 0)
      this += a
    else {
      if(index >= 0 && index < size) {
        val temp = (take(index + 1) += a) ++ drop(index + 1)
        clear
        this ++= temp
      }
    }
    this
  }
  
  def getElementIf(condition: A => Boolean): Option[A] = {
    var i = 0
    var result: Option[A] = None
    while(i < size && result.isEmpty) {
      if(condition(get(i).get))
        result = get(i)
      else
        i += 1
    }
    result
  }
  
  def getElementIndexIf(condition: A => Boolean): Int = {
    var i = 0
    var found = false
    while(i < size && !found) {
      if(condition(get(i).get))
        found = true
      else
        i += 1
    }
    if(found) i else -1
  }
  
  def elementExists(condition: A => Boolean): Boolean = {
    var i = 0
    var found = false
    while(i < size && !found) {
      if(condition(get(i).get))
        found = true
      else
        i += 1
    }
    found
  }
}