package com.deameamo.muse.longman.analysis

import scala.collection.mutable

object HeadConfigAnalyzer {

  def getEntryNNPConfig(form: String, entry: Entry, refPersonNumber: String, originality: String): NNPConfig = {
    def buildConfig(hwd: String, grams: mutable.MutableList[Gram]): NNPConfig = {
      val config = NNPConfig(hwd, refPersonNumber, originality)
      grams.foreach(gram => {
        if (gram.key == Grams.CAT && gram.value == Grams.COUNTABLE)
          config.countability += AspectValue.CNT
        else if (gram.key == Grams.CAT && gram.value == Grams.UNCOUNTABLE)
          config.countability += AspectValue.UNCNT
        else if (gram.key == Grams.INF &&
          (gram.value == Grams.PLURAL || gram.value == Grams.SINGULAR) &&
          gram.degree == Grams.ALWAYS)
          config.countability += AspectValue.CNT
      })
      if (config.countability.isEmpty) {
        config.countability += AspectValue.CNT
        config.countability += AspectValue.UNCNT
      }
      if (config.isUncountable && config.personNumber == AspectValue.P3PL) {
        if (config.isAbsoluteUncountable) {
          if (hwd == form)
            config.personNumber = AspectValue.P3SING
          else
            config.errors += Error(AspectName.PERSON_NUMBER, AspectValue.P3PL, AspectValue.P3SING)
        }
        else {
          config.removeUncountable()
        }
      }
      if (config.isCountable) {
        if (config.personNumber == AspectValue.P3PL) {
          val correct = Irregulars.getPluralForm(form, entry.irregulars)
          if (correct != form) {
            config.errors += Error(AspectName.FORM, form, correct)
          }
          else {
            config.form = entry.hwd
          }
        }
        if (exists(grams, Grams.INF, Grams.PLURAL, Grams.ALWAYS) && config.personNumber == AspectValue.P3SING)
          config.errors += Error(AspectName.PERSON_NUMBER, AspectValue.P3SING, AspectValue.P3PL)
        if (exists(grams, Grams.INF, Grams.SINGULAR, Grams.ALWAYS) && config.personNumber == AspectValue.P3PL)
          config.errors += Error(AspectName.PERSON_NUMBER, AspectValue.P3PL, AspectValue.P3SING)
      }
      config
    }

    val hwdConfig = buildConfig(entry.hwd, entry.grams(entry.hwd))
    val formConfig = {
      val formGrams = entry.grams(form)
      if (formGrams.nonEmpty) {
        buildConfig(form, formGrams)
      }
      else
        null
    }
    if (hwdConfig.errors.isEmpty)
      hwdConfig
    else {
      if (formConfig != null && formConfig.errors.isEmpty)
        formConfig
      else
        hwdConfig
    }
  }

  def getUnitNNPConfig(form: String, trueForm: String, unit: EntryUnit, entryConfig: NNPConfig): NNPConfig = {
    def buildConfig(configForm: String, grams: mutable.MutableList[Gram]): NNPConfig = {
      val config = NNPConfig(configForm, entryConfig.personNumber, entryConfig.originality)
      // determine countability
      grams.foreach(gram => {
        if (gram.key == Grams.CAT && gram.value == Grams.COUNTABLE)
          config.countability += AspectValue.CNT
        else if (gram.key == Grams.CAT && gram.value == Grams.UNCOUNTABLE)
          config.countability += AspectValue.UNCNT
        else if (gram.key == Grams.INF &&
          (gram.value == Grams.PLURAL || gram.value == Grams.SINGULAR) &&
          gram.degree == Grams.ALWAYS)
          config.countability += AspectValue.CNT
        else if (gram.key == Grams.INF && gram.value == Grams.BIPOLAR && gram.degree == Grams.ALWAYS) {
          config.countability += AspectValue.CNT
          config.personNumber = AspectValue.P3BI
        }
      })
      if (config.countability.isEmpty) {
        config.countability ++= entryConfig.countability
      }
      if (config.isUncountable && config.personNumber == AspectValue.P3PL) {
        if (config.isAbsoluteUncountable) {
          if (configForm == form)
            config.personNumber = AspectValue.P3SING
          else
            config.errors += Error(AspectName.PERSON_NUMBER, AspectValue.P3PL, AspectValue.P3SING)
        }
        else {
          config.removeUncountable()
        }
      }
      if (config.isCountable) {
        if (config.personNumber == AspectValue.P3PL) {
          val correct = Irregulars.getPluralForm(form, unit.irregulars)
          if (correct != form) {
            config.errors += Error(AspectName.FORM, form, correct)
          }
          else {
            config.form = trueForm
          }
        }
        if (exists(grams, Grams.INF, Grams.PLURAL, Grams.ALWAYS) && config.personNumber == AspectValue.P3SING)
          config.errors += Error(AspectName.PERSON_NUMBER, AspectValue.P3SING, AspectValue.P3PL)
        if (exists(grams, Grams.INF, Grams.SINGULAR, Grams.ALWAYS) && config.personNumber == AspectValue.P3PL)
          config.errors += Error(AspectName.PERSON_NUMBER, AspectValue.P3PL, AspectValue.P3SING)
      }
      config
    }

    val hwdConfig = buildConfig(unit.hwd, unit.grams(unit.hwd))
    val formConfig = {
      val formGrams = unit.grams(form)
      if (formGrams.nonEmpty) {
        buildConfig(form, formGrams)
      }
      else
        null
    }

    if (hwdConfig.errors.isEmpty)
      hwdConfig
    else {
      if (formConfig != null && formConfig.errors.isEmpty)
        formConfig
      else
        null
    }
  }

  def getEntryVBPConfig(form: String,
                        entry: Entry,
                        isStem: String,
                        function: String,
                        tense: String,
                        personNumber: String,
                        originality: String): VBPConfig = {
    val config = VBPConfig(entry.hwd, isStem, function, tense, personNumber, originality)
    val grams = {
      val hwdGram = entry.grams(entry.hwd)
      if (hwdGram.nonEmpty) hwdGram else entry.grams(form)
    }
    grams.foreach(gram => {
      if (gram.key == Grams.CAT && gram.value == Grams.TRANSITIVE)
        config.transitivity += AspectValue.TRANS
      else if (gram.key == Grams.CAT && gram.value == Grams.INTRANSITIVE)
        config.transitivity += AspectValue.INTRANS
    })
    if (config.transitivity.isEmpty) {
      config.transitivity += AspectValue.TRANS
      config.transitivity += AspectValue.INTRANS
    }
    config
  }

  def getUnitVBPConfig(form: String, hwd: String, unit: EntryUnit, entryConfig: VBPConfig): VBPConfig = {
    val config = VBPConfig(hwd, entryConfig.isStem, entryConfig.function,
      entryConfig.tense, entryConfig.personNumber, entryConfig.originality)
    val grams = {
      val hwdGram = unit.grams(unit.hwd)
      if (hwdGram.nonEmpty) hwdGram else unit.grams(form)
    }
    grams.foreach(gram => {
      if (gram.key == Grams.CAT && gram.value == Grams.TRANSITIVE)
        config.transitivity += AspectValue.TRANS
      else if (gram.key == Grams.CAT && gram.value == Grams.INTRANSITIVE)
        config.transitivity += AspectValue.INTRANS
    })
    if (config.transitivity.isEmpty) {
      config.transitivity ++= entryConfig.transitivity
    }
    config
  }

  private def exists(grams: mutable.MutableList[Gram], key: String, value: String, degree: Int): Boolean = {
    var does = false
    var i = 0
    while (i < grams.size && !does) {
      val gram = grams.apply(i)
      does = gram.key == key && gram.value == value && gram.degree == degree
      i += 1
    }
    does
  }
}

case class NNPConfig(var form: String, var personNumber: String, originality: String) {
  val genre: String = AspectValue.SBSTH
  val countability = new mutable.HashSet[String]
  val errors = new mutable.MutableList[Error]

  def isAbsoluteUncountable: Boolean = countability.size == 1 && countability.head == AspectValue.UNCNT

  def isCountable: Boolean = countability.contains(AspectValue.CNT)

  def isUncountable: Boolean = countability.contains(AspectValue.UNCNT)

  def removeUncountable(): Boolean = countability.remove(AspectValue.UNCNT)

  override def toString: String = s"NNPConfig: $form[${countability.mkString("[", ",", "]")}," +
    s"$personNumber,$originality,${if (errors.isEmpty) "NO_ERROR" else errors.mkString(",")}]"
}

case class VBPConfig(var form: String,
                     isStem: String,
                     function: String,
                     tense: String,
                     personNumber: String,
                     originality: String) {
  val transitivity = new mutable.HashSet[String]
  val voice: String = if (function == AspectValue.PREDICATE) AspectValue.ACT else AspectValue.UNDEF
  val positivity: Head = if (function == AspectValue.PREDICATE) Head.POSHead else Head.UNDEFHead
  val continuance: String = if (function == AspectValue.PREDICATE) AspectValue.GEN else AspectValue.UNDEF

  def isTransitive: Boolean = transitivity.contains(AspectValue.TRANS)

  def isIntransitive: Boolean = transitivity.contains(AspectValue.INTRANS)

  override def toString: String = s"VBPConfig: $form[$isStem,$function,$tense,$personNumber," +
    s"${transitivity.mkString("[", ",", "]")},$originality]"
}

case class VBAConfig(var form: String,
                     isStem: String,
                     function: String,
                     tense: String,
                     personNumber: String,
                     originality: String) {
  val positivity: String = if (function == AspectValue.PREDICATE) AspectValue.POS else AspectValue.UNDEF
  val continuance: String = if (function == AspectValue.PREDICATE) AspectValue.GEN else AspectValue.UNDEF

  override def toString = s"VBPConfig: $form[$isStem,$function,$tense,$personNumber,$originality]"
}
