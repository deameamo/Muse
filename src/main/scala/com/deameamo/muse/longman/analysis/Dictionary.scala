package com.deameamo.muse.longman.analysis

import java.io.File
import javax.xml.parsers.DocumentBuilderFactory

import com.deameamo.commons.log.DmmLogger.logger._
import com.deameamo.commons.util.FileUtil
import org.w3c.dom.Element

import scala.collection.mutable

object Dictionary {

  var count = 0

  def main(args: Array[String]) {
    EntryUnit.targetPOS = POS.ADJECTIVE
    load()

    info(s"count=$count")

    printlnGramSet("adjective")
    //    collectFunctionWords(POS.CONJUNCTION, "conjunction")
    collectFunctionWords(POS.ADVERB, "adverb")

    info(s"inGloss=${EntryUnit.inGloss}, ex=${EntryUnit.example}")
    info("done")
  }

  private def printlnGramSet(fileName: String) {
    var out = FileUtil.getWriter(s"longman/analysis/gramSet/${fileName}_lex")
    EntryUnit.lexGramMap.foreach(pair => {
      out.println(s"${pair._1} => ${pair._2.name}")
    })
    FileUtil.closeWriter(out)
    out = FileUtil.getWriter(s"longman/analysis/gramSet/${fileName}_grd")
    EntryUnit.grdGramMap.foreach(pair => {
      out.println(s"${pair._1} => ${pair._2.name}")
    })
    FileUtil.closeWriter(out)
  }

  private def collectFunctionWords(pos: String, fileName: String) {
    val out = FileUtil.getWriter(s"longman/analysis/functionWords/$fileName")
    entries.foreach(entry => {
      if (entry.posList.contains(pos)) {
        out.println(entry.name)
      }
    })
    FileUtil.closeWriter(out)
  }

  private val builder = DocumentBuilderFactory.newInstance.newDocumentBuilder

  private val formMap = new mutable.HashMap[String, mutable.MutableList[Entry]]

  private val phrasalVerbMap = new mutable.HashMap[String, mutable.MutableList[Entry]]

  private val mweMap = new mutable.HashMap[String, mutable.MutableList[Entry]]

  val entries = new mutable.MutableList[Entry]

  var loaded = false

  def indexHwd(hwd: String, entry: Entry) {
    if (entry.category == Entry.WORD && !hwd.contains(" ")) {
      if (!formMap.contains(hwd)) {
        formMap.put(hwd, new mutable.MutableList[Entry])
      }
      formMap.apply(hwd) += entry
    }
  }

  def indexPhrasalVerb(hwd: String, entry: Entry) {
    if (entry.category == Entry.PHRVB) {
      if (!phrasalVerbMap.contains(entry.mainVerb)) {
        phrasalVerbMap.put(entry.mainVerb, new mutable.MutableList[Entry])
      }
      phrasalVerbMap.apply(entry.mainVerb) += entry
    }
  }

  def indexMWE(hwd: String, entry: Entry) {
    if (entry.category == Entry.WORD && hwd.contains(" ")) {
      val firstWord = hwd.split(" ").head
      if (!mweMap.contains(firstWord)) {
        mweMap.put(firstWord, new mutable.MutableList[Entry])
      }
      mweMap.apply(firstWord) += entry
    }
  }

  def lookupForm(form: String, pos: String): mutable.MutableList[Entry] = {
    load()
    val list = new mutable.MutableList[Entry]
    if (formMap.contains(form)) {
      formMap.apply(form).foreach(entry => {
        if (entry.isPos(pos))
          list += entry
      })
    }
    list
  }

  def lookupForm(form: String): mutable.MutableList[Entry] = {
    load()
    val list = new mutable.MutableList[Entry]
    if (formMap.contains(form)) {
      formMap.apply(form).foreach(entry => {
        list += entry
      })
    }
    list
  }

  def lookupPhrasalVerb(mainVerb: String): mutable.MutableList[Entry] = {
    load()
    val list = new mutable.MutableList[Entry]
    if (phrasalVerbMap.contains(mainVerb)) {
      phrasalVerbMap.apply(mainVerb).foreach(entry => {
        list += entry
      })
    }
    list
  }

  def lookupMWE(firstWord: String): mutable.MutableList[Entry] = {
    load()
    val list = new mutable.MutableList[Entry]
    if (mweMap.contains(firstWord)) {
      mweMap.apply(firstWord).foreach(entry => {
        list += entry
      })
    }
    list
  }

  def load(): Unit = {
    if (loaded)
      return
    val t0 = System.currentTimeMillis
    info("Start loading dictionary.")
    val doc = builder.parse(new File(Resource.PATH_DICTIONARY))
    val root = doc.getDocumentElement
    for (i <- 0 until root.getChildNodes.getLength) {
      val child = root.getChildNodes.item(i)
      child.getFirstChild
      if (child.getNodeName == "entry") {
        entries += new Entry(child.asInstanceOf[Element])
      }
    }
    info(s"Dictionary loaded in ${System.currentTimeMillis - t0}ms.")
    loaded = true
  }
}
