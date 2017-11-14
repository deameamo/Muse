package com.deameamo.muse.longman.analysis

object AspectName {
  val FORM = "FORM"
  val ROLE = "ROLE"
  val COUNTABILITY = "COUNTABILITY"
  val PERSON_NUMBER = "PERSON_NUMBER"
  val GENRE = "GENRE"
  val REF = "REF"
  val DET = "DET"
  val PREDET = "PREDET"
  val PREMOD = "PREMOD"
  val POSTMOD = "POSTMOD"
  val VAR = "VAR"
  val IS_STEM = "IS_STEM"
  val TRANSITIVITY = "TRANSITIVITY"
  val FUNCTION = "FUNCTION"
  val VOICE = "VOICE"
  val POSITIVITY = "POSITIVITY"
  val CONTINUANCE = "CONTINUANCE"
  val MODALITY = "MODALITY"
  val TENSE = "TENSE"
  val DEGREE = "DEGREE"
  val QW_TYPE = "QW_TYPE"
  val QW_MARK = "QW_MARK"
  val TARGET = "TARGET"
  val VAR_TYPO = "VAR_TYPO"
  val SUBJ = "SUBJ"
  val PRED = "PRED"
}

object AspectValue {
  val UNDEF = "UNDEF"
  //originality
  val ORIG = "ORIG"
  val INORIG = "INORIG"
  // role
  val REG = "REG"
  val NOM = "NOM"
  val OBL = "OBL"
  val POSS = "POSS"
  val POSSNN = "POSSNN"
  val REFL = "REFL"
  // countability
  val CNT = "CNT"
  val UNCNT = "UNCNT"
  val BICNT = "BICNT"
  // personNumber
  val P1SING = "P1SING"
  val P2 = "P2"
  val P3SING = "P3SING"
  val P1PL = "P1PL"
  val P3PL = "P3PL"
  val P3BI = "P3BI"
  val PL = "PL"
  val COMMON = "COMMON"
  val ALL = "ALL"
  // genre
  val STH = "STH"
  val SB = "SB"
  val SBSTH = "SBSTH"
  // determiner
  val DA = "DA"
  val IA = "IA"
  val NODT = "NODT"
  val DT = "DT"
  // isStem
  val STEM = "STEM"
  val INFL = "INFL"
  // function
  val INFINITIVE = "INFINITIVE"
  val PREDICATE = "PREDICATE"
  val PRESPART = "PRESPART"
  val PASTPART = "PASTPART"
  // voice
  val ACT = "ACT"
  val PASS = "PASS"
  // positivity
  val POS = "POS"
  val NEG = "NEG"
  val NOT = "NOT"
  // continuance
  val GEN = "GEN"
  val PROG = "PROG"
  val PERF = "PERF"
  // tense
  val PRES = "PRES"
  val PAST = "PAST"
  val FUT = "FUT"
  // transitivity
  val TRANS = "TRANS"
  val INTRANS = "INTRANS"
  // modality
  val NOMD = "NOMD"
  // degree
  val POSI = "POSI"
  val COMP = "COMP"
  val SUPE = "SUPE"
  // question words
  val PLAIN = "PLAIN"
  val WHAT = "WHAT"
  val WHOM = "WHOM"
  val THAT = "THAT"

  // nos
  val NOMOD = "NOMOD"
  val NOPREMOD = "NOPREMOD"
  val NOPOSTMOD = "NOPOSTMOD"

  val NNP_VALUES = Seq(NODT, DA, IA, P3PL)
  val VBP_VALUES = Seq(PRES, PAST, FUT)
  val ROLES = Seq(REG, NOM, OBL, POSS, POSSNN)
  val PERSON_NUMBERS = Seq(P1SING, P2, P3SING, P1PL, P3PL, COMMON, ALL)
  val GENRES = Seq(SBSTH, SB, STH)
}
