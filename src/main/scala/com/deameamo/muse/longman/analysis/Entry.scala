package com.deameamo.muse.longman.analysis

import org.w3c.dom.Element

import scala.collection.mutable

object Entry {
  val WORD = "word"
  val PHRVB = "phrVb"
  val DERIV = "deriv"
}

class Entry(xmlEle: Element) {
  var hwd: String = _
  var transRule: String = _
  var category: String = _
  var mainVerb: String = _
  val posList = new mutable.MutableList[String]
  var hyphenation: String = _
  var homnum: Int = 0
  val variants = new mutable.MutableList[Variant]
  val gramsMap = new mutable.HashMap[String, mutable.MutableList[Gram]]
  val registers = new mutable.MutableList[String]
  var geo: GEO = GEO.DEFAULT
  val senses = new mutable.MutableList[Sense]
  val irregulars = new mutable.MutableList[Irregular]
  var last = false
  var currVariant: Variant = _

  category = xmlEle.getAttribute("category")
  for (i <- 0 until xmlEle.getChildNodes.getLength) {
    val child = xmlEle.getChildNodes.item(i)
    child.getNodeName match {
      case "head" =>
        val headEle = child.asInstanceOf[Element]
        for (j <- 0 until headEle.getChildNodes.getLength) {
          val headItem = headEle.getChildNodes.item(j)
          headItem.getNodeName match {
            case "hwd" =>
              hwd = headItem.getTextContent
            case "mainVerb" => mainVerb = headItem.getTextContent
            case "homnum" => homnum = headItem.getTextContent.toInt
            case "pos" => posList ++= POS.transfer(headItem.getTextContent.split('|'))
            case "hyphenation" => hyphenation = headItem.getTextContent
            case "variant" =>
              val variant = new Variant(headItem.asInstanceOf[Element])
              Dictionary.indexHwd(variant.hwd, this)
              variants += variant
              currVariant = variant
            case "gram" =>
              if (currVariant == null) {
                gramsMap.put(hwd, new mutable.MutableList[Gram])
                headItem.getTextContent.split('|').foreach(gramsMap.apply(hwd) += Gram(_))
              }
              else {
                gramsMap.put(currVariant.hwd, new mutable.MutableList[Gram])
                headItem.getTextContent.split('|').foreach(gramsMap.apply(currVariant.hwd) += Gram(_))
              }
            case "register" => registers += headItem.getTextContent
            case "geo" => geo = GEO.getGEO(headItem.getTextContent)
            case "inflection" =>
              for (infIter <- 0 until headItem.getChildNodes.getLength) {
                val infItem = headItem.getChildNodes.item(infIter)
                if (infItem.getNodeName != "#text") {
                  irregulars += Irregular(infItem.getTextContent, infItem.getNodeName, this)
                }
              }
            case "transRule" => transRule = headItem.getTextContent
            case _ =>
          }
        }
      case "sense" => senses += new Sense(child.asInstanceOf[Element], this, s"sense_${senses.size}")
      case "tail" =>
      case _ =>
    }
  }

  if (gramsMap.size == 1 && !gramsMap.contains(hwd)) {
    gramsMap.put(hwd, gramsMap.head._2)
  }

  if (Letter.containsSpecialLetter(hwd)) {
    val variant = new Variant(Letter.transferToPlainWord(hwd))
    Dictionary.indexHwd(variant.hwd, this)
    variants += variant
    if (gramsMap.contains(hwd))
      gramsMap.put(variant.hwd, gramsMap.apply(hwd))
  }

  irregulars.foreach(Irregulars.index)
  senses.foreach(sense => {
    sense.irregulars.foreach(Irregulars.index)
    sense.subsenses.foreach(subsense => {
      subsense.irregulars.foreach(Irregulars.index)
    })
  })

  if (posList.contains(POS.NOUN))
    Dictionary.count += 1

  Dictionary.indexHwd(hwd, this)
  Dictionary.indexPhrasalVerb(hwd, this)
  Dictionary.indexMWE(hwd, this)
  EntryUnit.collectEntryGram(gramsMap, posList, this)
  //  EntryUnit.checkGramsMap(gramsMap, name)

  def isPos(pos: String): Boolean = {
    if (pos == POS.VERB)
      posList.contains(POS.VERB) || posList.contains(POS.AUX) || posList.contains(POS.MODAL) || posList.contains(POS.PRESPART)
    else
      posList.contains(pos)
  }

  def grams(word: String): mutable.MutableList[Gram] = if (gramsMap.isDefinedAt(word)) gramsMap.apply(word) else EntryUnit.EMPTY_GRAMS

  def name = s"$hwd${if (homnum == 0) "" else s"_$homnum"}"

  override def toString = s"$name [${posList.mkString(",")}]"

  def getFirstSenseIndex: String = {
    if (senses.nonEmpty) {
      if (senses.head.subsenses.isEmpty)
        senses.head.toString
      else
        senses.head.subsenses.head.toString
    }
    else
      null
  }
}

abstract class EntryUnit(val entry: Entry, val index: String) {
  val typo: String
  var hwd: String = _
  var lexUnit: String = _
  var ruleDesc: String = _
  var gloss: String = _
  val examples = new mutable.MutableList[String]
  val gramsMap = new mutable.HashMap[String, mutable.MutableList[Gram]]
  val variants = new mutable.MutableList[Variant]
  val irregulars = new mutable.MutableList[Irregular]

  def grams(word: String): mutable.MutableList[Gram] = if (gramsMap.isDefinedAt(word)) gramsMap.apply(word) else EntryUnit.EMPTY_GRAMS

  override def toString = s"${entry.name}->$index"
}

object EntryUnit {
  val SENSE = "Sense"
  val SUBSENSE = "Subsense"
  val EXA = "Exa"

  val EMPTY_GRAMS = new mutable.MutableList[Gram]

  var inGloss = false

  var example = ""

  var symbol = "["

  def checkGloss(gloss: String, entry: Entry) {
    inGloss = inGloss || (gloss != null && gloss.contains(symbol))
  }

  def checkGramsMap(gramsMap: mutable.HashMap[String, mutable.MutableList[Gram]], id: String) {
    gramsMap.values.foreach(list => {
      var i = 0
      var isTarget = false
      while (i < list.size && !isTarget) {
        val gram = list.apply(i)
        if (gram.key == Grams.BEF || gram.key == Grams.AFT) {
          isTarget = true
        }
        i += 1
      }
      if (isTarget && list.size > 1) {
        println(s"problematic gramsMap found: $id: ${list.mkString("[", ",", "]")}")
      }
    })
  }

  val lexGramMap = new mutable.HashMap[String, Entry]
  val grdGramMap = new mutable.HashMap[String, Entry]

  var targetPOS: String = _

  def collectEntryGram(gramsMap: mutable.HashMap[String, mutable.MutableList[Gram]], posList: mutable.MutableList[String], entry: Entry) {
    if (posList.contains(targetPOS)) {
      gramsMap.values.foreach(list => {
        list.foreach(gram => {
          lexGramMap.put(gram.toString, entry)
        })
      })
    }
  }

  def collectUnitGram(gramsMap: mutable.HashMap[String, mutable.MutableList[Gram]], posList: mutable.MutableList[String], unit: EntryUnit) {
    if (posList.contains(targetPOS)) {
      if (unit.lexUnit == null || unit.lexUnit == unit.entry.hwd) {
        gramsMap.values.foreach(list => {
          list.foreach(gram => {
            lexGramMap.put(gram.toString, unit.entry)
          })
        })
      }
      else {
        gramsMap.values.foreach(list => {
          list.foreach(gram => {
            grdGramMap.put(gram.toString, unit.entry)
          })
        })
      }
    }
  }

  def containsTargetGram(gramsMap: mutable.HashMap[String, mutable.MutableList[Gram]]): Boolean = {
    if (gramsMap.nonEmpty) {
      var contains = false
      gramsMap.values.foreach(list => {
        list.foreach(gram => {
          contains = contains || (gram.key == Grams.BEF || gram.key == Grams.AFT)
        })
      })
      contains
    }
    else
      false
  }
}

class Sense(htmlEle: Element, entry: Entry, index: String) extends EntryUnit(entry, index) {
  val typo: String = EntryUnit.SENSE
  var subsenses = new mutable.MutableList[Subsense]
  val exas = new mutable.MutableList[Exa]
  var currVariant: Variant = _
  hwd = entry.hwd

  for (i <- 0 until htmlEle.getChildNodes.getLength) {
    val child = htmlEle.getChildNodes.item(i)
    child.getNodeName match {
      case "lexUnit" =>
        lexUnit = child.getTextContent
        //        Dictionary.indexHwd(lexUnit, entry)
        hwd = lexUnit
      case "ruleDesc" => ruleDesc = child.getTextContent
      case "def" => gloss = child.getTextContent
      case "variant" =>
        val variant = new Variant(child.asInstanceOf[Element])
        Dictionary.indexHwd(variant.hwd, entry)
        variants += variant
        currVariant = variant
      case "gram" =>
        if (currVariant == null) {
          gramsMap.put(hwd, new mutable.MutableList[Gram])
          child.getTextContent.split('|').foreach(gramsMap.apply(hwd) += Gram(_))
        }
        else {
          gramsMap.put(currVariant.hwd, new mutable.MutableList[Gram])
          child.getTextContent.split('|').foreach(gramsMap.apply(currVariant.hwd) += Gram(_))
        }
      case "example" => examples += child.getTextContent
      case "subsense" => subsenses += new Subsense(child.asInstanceOf[Element], entry, s"$index->subsense_${subsenses.size}", this)
      case "inflection" =>
        for (infIter <- 0 until child.getChildNodes.getLength) {
          val infItem = child.getChildNodes.item(infIter)
          if (infItem.getNodeName != "#text") {
            irregulars += Irregular(infItem.getTextContent, infItem.getNodeName, entry)
          }
        }
      case "colloExa" => exas += new Exa(child.asInstanceOf[Element], entry, s"$index->exa_${exas.size}", this)
      case "gramExa" => exas += new Exa(child.asInstanceOf[Element], entry, s"$index->exa_${exas.size}", this)
      case _ =>
    }
  }

  //  if(gramsMap.size > 1) {
  //    println(s"${entry.name}->${index}")
  //    gramsMap.foreach(pair => {
  //      println(s"  ${pair._1}:${pair._2}")
  //    })
  //  }
  if (gramsMap.size == 1 && !gramsMap.contains(hwd)) {
    gramsMap.put(hwd, gramsMap.head._2)
  }

  //  if(entry.posList.contains(POS.NOUN) && lexUnit == null)
  //    Dictionary.count += 1

  EntryUnit.checkGloss(gloss, entry)
  EntryUnit.collectUnitGram(gramsMap, entry.posList, this)
  //  EntryUnit.checkGramsMap(gramsMap, s"${entry.name}->${index}")
  //  if(lexUnit != null && EntryUnit.containsTargetGram(gramsMap))
  //    println(s"${entry.name}->${index}")
}

class Subsense(htmlEle: Element, entry: Entry, index: String, val parent: Sense) extends EntryUnit(entry, index) {
  val typo: String = EntryUnit.SUBSENSE
  val exas = new mutable.MutableList[Exa]
  var currVariant: Variant = _
  lexUnit = parent.hwd
  //  lexUnit = null
  hwd = lexUnit

  for (i <- 0 until htmlEle.getChildNodes.getLength) {
    val child = htmlEle.getChildNodes.item(i)
    child.getNodeName match {
      case "lexUnit" =>
        lexUnit = child.getTextContent
        //        Dictionary.indexHwd(lexUnit, entry)
        hwd = lexUnit
      case "ruleDesc" => ruleDesc = child.getTextContent
      case "def" => gloss = child.getTextContent
      case "variant" =>
        val variant = new Variant(child.asInstanceOf[Element])
        Dictionary.indexHwd(variant.hwd, entry)
        variants += variant
        currVariant = variant
      case "gram" =>
        if (currVariant == null) {
          gramsMap.put(hwd, new mutable.MutableList[Gram])
          child.getTextContent.split('|').foreach(gramsMap.apply(hwd) += Gram(_))
        }
        else {
          gramsMap.put(currVariant.hwd, new mutable.MutableList[Gram])
          child.getTextContent.split('|').foreach(gramsMap.apply(currVariant.hwd) += Gram(_))
        }
      case "example" => examples += child.getTextContent
      case "inflection" =>
        for (infIter <- 0 until child.getChildNodes.getLength) {
          val infItem = child.getChildNodes.item(infIter)
          if (infItem.getNodeName != "#text") {
            irregulars += Irregular(infItem.getTextContent, infItem.getNodeName, entry)
          }
        }
      case "colloExa" => exas += new Exa(child.asInstanceOf[Element], entry, s"$index->exa_${exas.size}", this)
      case "gramExa" => exas += new Exa(child.asInstanceOf[Element], entry, s"$index->exa_${exas.size}", this)
      case _ =>
    }
  }

  if (this.ruleDesc == null && parent.ruleDesc != null)
    this.ruleDesc = parent.ruleDesc

  //  if(gramsMap.size > 1) {
  //    println(s"${entry.name}->${index}")
  //    gramsMap.foreach(pair => {
  //      println(s"  ${pair._1}:${pair._2}")
  //    })
  //  }
  if (gramsMap.size == 1 && !gramsMap.contains(hwd)) {
    gramsMap.put(hwd, gramsMap.head._2)
  }

  //  if(entry.posList.contains(POS.NOUN) && lexUnit == null)
  //    Dictionary.count += 1

  EntryUnit.checkGloss(gloss, entry)
  EntryUnit.collectUnitGram(gramsMap, entry.posList, this)
  //  EntryUnit.checkGramsMap(gramsMap, s"${entry.name}->${index}")
  //  if(lexUnit != null && EntryUnit.containsTargetGram(gramsMap))
  //    println(s"${entry.name}->${index}")
}

class Exa(htmlEle: Element, entry: Entry, index: String, val parent: EntryUnit) extends EntryUnit(entry, index) {
  val typo: String = EntryUnit.EXA
  var geo: GEO = GEO.DEFAULT

  for (i <- 0 until htmlEle.getChildNodes.getLength) {
    val child = htmlEle.getChildNodes.item(i)
    child.getNodeName match {
      case "hwd" =>
        hwd = child.getTextContent
        lexUnit = hwd
      //        Dictionary.indexHwd(hwd, entry)
      case "ruleDesc" => ruleDesc = child.getTextContent
      case "gloss" => gloss = child.getTextContent
      case "geo" => geo = GEO.getGEO(child.getTextContent)
      case "example" => examples += child.getTextContent
      case "variant" =>
        val variant = new Variant(child.asInstanceOf[Element])
        Dictionary.indexHwd(variant.hwd, entry)
        variants += variant
      case _ =>
    }
  }
}

class Variant(val hwd: String, val cat: String, val geo: GEO, val ruleDesc: String) {
  def this(htmlEle: Element) = this(
    htmlEle.getTextContent.replaceAll("‧", ""),
    htmlEle.getAttribute("cat"),
    GEO.getGEO(htmlEle.getAttribute("geo")),
    if (htmlEle.hasAttribute("ruleDesc")) htmlEle.getAttribute("ruleDesc") else null)

  def this(hwd: String) = this(hwd, "ORTHVAR", GEO.DEFAULT, "")
}
