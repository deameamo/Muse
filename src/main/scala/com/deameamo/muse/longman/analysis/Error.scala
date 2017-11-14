package com.deameamo.muse.longman.analysis

case class Error(name: String, givenValue: String, correctValue: String)

object Debugger {
  var ON = false

  def on() {
    ON = true
  }

  def off() {
    ON = false
  }

  def println(msg: String) {
    if (ON)
      System.out.println(msg)
  }

  def debug(perform: Unit => Unit) {
    if (ON)
      perform()
  }

  var head: Head = _

  var pre0Phrase: Phrase = _

  var pre0: Head = _
}