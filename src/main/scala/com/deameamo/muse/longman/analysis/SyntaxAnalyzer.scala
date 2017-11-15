package com.deameamo.muse.longman.analysis

import com.deameamo.commons.util.Matrix

import scala.collection.mutable

object SyntaxAnalyzer {

  def main(args: Array[String]) {
    var sentence = ""
    //    sentence = "give students a great book"
    //    sentence = "give the student an idea about the future"
    //    sentence = "great idea about"
    //    sentence = "participation by students in community"
    //    sentence = "give a watch to students"
    //    sentence = "I love blind dates"
    //    sentence = "very happy student"
    //    sentence = "I have book"
    //    sentence = "absorption of a book"
    //    sentence = "I am admiring a very happy little student"
    //    sentence = "what do I give students a great idea about job"
    //    sentence = "very happy very little student"
    //    sentence = "give gift to you"
    //    sentence = "what kind of books do you want"
    //    sentence = "I will give a book"
    //    sentence = "know that I give you"
    //    sentence = "I do not know you know the beast kills the man"
    //    sentence = "very good and long"
    //    sentence = "to the accompaniment of girls"
    //    sentence = "I am warned of the danger"
    //    sentence = "a book is given to me"
    //    sentence = "make friends with different people from different country of different age of different color with different hobbies of different background"
    sentence = "off the switch I"
    println(s"begin analysis: $sentence")
    val t0 = System.currentTimeMillis
    analyze(sentence)
    println(s"done in ${System.currentTimeMillis - t0}ms")
  }

  Dictionary.load()
  println("dictionary loaded")
  //  Thread.sleep(3000)

  def analyze(sentence: String) {
    val lexicals = LexicalAnalyzer.analyze(sentence)
    val bagOfHwds = new mutable.HashSet[String]
    lexicals.foreach(lexical => {
      bagOfHwds ++= lexical.getHwds
    })
    val matrix = new Matrix[Cell](lexicals.size, null)
    for (i <- lexicals.indices) {
      matrix.set(i, i, lexicals.apply(i).generateCell(bagOfHwds))
    }
    println(s"upgraders:")
    PhraseFactory.upgraders.foreach(println(_))
    for (i <- lexicals.indices) {
      PhraseFactory.addUpgraders(matrix.get(i)(i))
    }
    for (spanLength <- 1 until lexicals.size) {
      for (i <- 0 until lexicals.size - spanLength) {
        val row = i
        val col = i + spanLength
        val cell = new Cell
        matrix.set(row, col, cell)
        for (j <- 0 until col - row) {
          val preCell = matrix.get(row)(row + j)
          val postCell = matrix.get(row + j + 1)(col)
          println(s"========================================================================================")
          println(s"analyzing cell matrix[$row][$col]: pre=matrix[$row][${row + j}], post=matrix[${row + j + 1}][$col]")
          if (row == 0 && col == 3 && j == 0)
            Debugger.off()
          else
            Debugger.off()
          Debugger.debug(x => {
            println(s"========================================================================================")
            println(s"analyzing cell matrix[$row][$col]: pre=matrix[$row][${row + j}], post=matrix[${row + j + 1}][$col]")
          })
          preCell.phrases.foreach(a => {
            postCell.phrases.foreach(b => {
              val abPhrase = a.absolve(b, Rule.FORWARD)
              cell.addResultPhrase(abPhrase)
              cell.addResultPhrase(b.absolve(a, Rule.BACKWARD))
              if (abPhrase == null)
                RuleEngine.absolveByGeneralRules(a, b).foreach(phrase => cell.addResultPhrase(phrase))
            })
          })

          //          var id = 1
          //          cell.phrases.foreach(phrase => {
          //            if(phrase.coreHead != null) {
          //              println(s"${id}:")
          //              println(s"phrase.rule: ${phrase.rule}")
          //              phrase.coreHead.print
          //              println(s"phrase.headMap: ${phrase.headMap}")
          //            }
          //            else {
          //              println(s"${id}: no coreHead")
          //              println(s"phrase.rule: ${phrase.rule}")
          //              phrase.initHead.print
          //            }
          //            id += 1
          //          })

        }
        cell.postProcess()
        PhraseFactory.addUpgraders(cell)
      }
    }

    //    println(s"\n the best result is (out of ${matrix.get(0)(lexicals.size - 1).phrases.size})")
    //    var phraseId = 1
    //    val group = matrix.get(0)(lexicals.size - 1).organize.head
    //    group.foreach(phrase => {
    //      if(phrase.coreHead != null) {
    //        println(s"${phraseId}: quality=${phrase.coreHead.quality}")
    //        println(s"phrase.rule: ${phrase.rule}")
    //        phrase.coreHead.print(0, "")
    //      }
    //      else {
    //        println(s"${phraseId}: unfinished")
    //        println(phrase)
    //        if(phrase.initHead != null)
    //          phrase.initHead.print(0, "")
    //      }
    //      phraseId += 1
    //    })

    println(s"\nresult for cell(${0},${lexicals.size - 1}): Phrase.count=${Phrase.count}")
    var groupId = 1
    var phraseId = 1
    matrix.get(0)(lexicals.size - 1).organize.foreach(group => {
      println(s"group $groupId:")
      phraseId = 1
      group.foreach(phrase => {
        if (phrase.coreHead != null) {
          println(s"$phraseId: quality=${phrase.coreHead.quality}")
          println(s"phrase ${phrase.id}: rule= ${phrase.rule}")
          phrase.coreHead.print()
        }
        else {
          println(s"$phraseId: unfinished")
          println(phrase)
          if (phrase.initHead != null)
            phrase.initHead.print()
        }
        phraseId += 1
      })
      groupId += 1
    })

    //    println
    //    println("Phrases:")
    //    var id = 1
    //    matrix.get(0)(lexicals.size - 1).phrases.foreach(phrase => {
    //      if(phrase.coreHead != null) {
    //        println(s"${id}: quality=${phrase.coreHead.quality}")
    //        println(s"phrase ${phrase.id}: rule= ${phrase.rule}")
    //        phrase.coreHead.print
    //        phrase.deepCopy
    //      }
    //      else {
    //        println(s"${id}: unfinished, initHead.quality=${phrase.initHead.quality}")
    //        phrase.deepCopy
    //        println(phrase)
    //        if(phrase.initHead != null)
    //          phrase.initHead.print
    //      }
    //      id += 1
    //    })

  }

}
