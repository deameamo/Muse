package com.deameamo.commons.util

import scala.collection.mutable

class Matrix[A](numOfRow: Int, numOfCol: Int, default: A) {
  def this(size: Int, default: A) = this(size, size, default)

  val matrix = new mutable.MutableList[mutable.MutableList[A]]
  for (_ <- 0 until numOfRow) {
    val row = new mutable.MutableList[A]
    matrix += row
    for (_ <- 0 until numOfCol)
      row += default
  }

  def set(row: Int, col: Int, a: A): Unit = matrix.apply(row).update(col, a)

  def get(row: Int)(col: Int): A = matrix.apply(row).apply(col)
}
