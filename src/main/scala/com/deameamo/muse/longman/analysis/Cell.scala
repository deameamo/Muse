package com.deameamo.muse.longman.analysis

import java.util.Comparator

import com.deameamo.commons.log.DmmLogger.logger._
import com.deameamo.commons.util.ArrayList

import scala.collection.mutable

class Cell {

  val phrases = new ArrayList[Phrase]

  def organize: mutable.MutableList[mutable.MutableList[Phrase]] = {
    val list = new java.util.ArrayList[Phrase]
    phrases.foreach(phrase => {
      if (phrase.coreHead != null &&
        //          phrase.coreHead.typo != Head.CLAUSE &&
        (if (phrase.typo == Phrase.UPGRADER) phrase.rule.isComplete else true))
        list.add(phrase)
    })
    list.sort(new Comparator[Phrase] {
      def compare(a: Phrase, b: Phrase): Int = b.coreHead.quality - a.coreHead.quality
    })
    val groups = new mutable.MutableList[mutable.MutableList[Phrase]]
    var currGroupQuality = Integer.MIN_VALUE
    var group: mutable.MutableList[Phrase] = null

    def addIntoGroup(group: mutable.MutableList[Phrase], phrase: Phrase) {
      var target: Phrase = null
      var i = 0
      while (i < group.size && target == null) {
        val iter = group.apply(i)
        if (iter.coreHead.equalsTo(phrase.coreHead, rewritingAspects = true))
          target = group.apply(i)
        i += 1
      }
      if (target == null) {
        group += phrase
      }
      else {
        target.coreHead.mergeWith(phrase.coreHead)
      }
    }

    for (i <- 0 until list.size) {
      val phrase = list.get(i)
      if (phrase.coreHead.quality != currGroupQuality) {
        group = new mutable.MutableList[Phrase]
        groups += group
        currGroupQuality = phrase.coreHead.quality
      }
      addIntoGroup(group, phrase)
    }
    groups
  }

  def addResultPhrase(incoming: Phrase) {
    if (incoming != null) {
      incoming.typo match {
        case Phrase.STRUCTURER => phrases += incoming
        case Phrase.UPGRADER =>
          var target: Phrase = null
          phrases.foreach(phrase => {
            if (phrase.rule.equalsTo(incoming.rule))
              target = phrase
          })
          if (target == null) {
            phrases += incoming
          }
          else {
            target.units ++= incoming.units
          }
        case Phrase.MWE =>
          var target: Phrase = null
          phrases.foreach(phrase => {
            if (phrase.typo == Phrase.MWE &&
              phrase.asInstanceOf[MWEPhrase].entry == incoming.asInstanceOf[MWEPhrase].entry &&
              phrase.asInstanceOf[MWEPhrase].rule.toString == incoming.asInstanceOf[MWEPhrase].rule.toString)
              target = phrase
          })
          if (target == null) {
            phrases += incoming
          }
          else {
            target.units ++= incoming.units
//            info(s"addResultPhrase: adding to target")
          }
      }
    }
  }

  def postProcess(): Unit = {
    val structurers = new mutable.MutableList[StructurerPhrase]
    val completeUpgraders = new mutable.MutableList[UpgraderPhrase]
    val incompleteUpgraders = new mutable.MutableList[UpgraderPhrase]
    val mwePhrases = new mutable.MutableList[MWEPhrase]
    phrases.foreach(phrase => {
      phrase.typo match {
        case Phrase.STRUCTURER => structurers += phrase.asInstanceOf[StructurerPhrase]
        case Phrase.UPGRADER =>
          if (phrase.rule.isComplete)
            completeUpgraders += phrase.asInstanceOf[UpgraderPhrase]
          else
            incompleteUpgraders += phrase.asInstanceOf[UpgraderPhrase]
        case Phrase.MWE => mwePhrases += phrase.asInstanceOf[MWEPhrase]
      }
    })
    val upgradedStructurers = new mutable.MutableList[Phrase]
    if (completeUpgraders.nonEmpty) {
      //      structurers.foreach(_.coreHead.print)
      structurers.foreach(phrase => {
        upgradedStructurers ++= phrase.upgradeByUpgraders(completeUpgraders)
      })
    }
    else {
      upgradedStructurers ++= structurers
    }
    upgradedStructurers.foreach(phrase => {
      phrase.asInstanceOf[StructurerPhrase].addCandidateUpgraders(incompleteUpgraders)
    })
    // remove complete upgraders. After upgrading structurers, they are not useful any more.
    phrases.clear
    var target: Phrase = null
    var i = 0
    upgradedStructurers.foreach(adding => {
      target = null
      i = 0
      while (i < phrases.size && target == null) {
        val added = phrases.apply(i)
        if (added.rule.isComplete && adding.rule.isComplete && added.coreHead.equalsTo(adding.coreHead))
          target = added
        i += 1
      }
      if (target == null) {
        phrases += adding
      }
      else {
        target.units ++= adding.units
      }

      //      phrases += adding
    })
    phrases ++= incompleteUpgraders
    transferMWEPhrases(mwePhrases)
    Phrase.count += phrases.size
  }

  def transferMWEPhrases(mwePhrases: mutable.MutableList[MWEPhrase]) {
    mwePhrases.foreach(mwePhrase => {
      if (mwePhrase.rule.status == Rule.DONE) {
        addPhrases(mwePhrase.transferMWE)
      }
      else {
        phrases += mwePhrase
      }
    })
  }

  def addPhrases(newPhrases: mutable.MutableList[Phrase]) {
    newPhrases.foreach(incoming => {
      if (incoming.initHead == null) {
        info(s"Cell.addPhrases: incoming.initHead==null ======================================================")
        phrases += incoming
      }
      else
        addUnitPhrase(incoming)
    })
  }

  private def addUnitPhrase(incoming: Phrase) {
    var target: Phrase = null
    var i = 0
    while (i < phrases.size && target == null) {
      val iter = phrases.apply(i)
      if (iter.initHead.equalsTo(incoming.initHead))
        target = iter
      i += 1
    }
    if (target == null) {
      incoming.updateInitHeadId()
      phrases += incoming
      Phrase.count += 1
    }
    else {
      target.units ++= incoming.units
    }
  }
}
