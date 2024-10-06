// https://leetcode.com/problems/sentence-similarity-iii/

object SentenceSimilarity:

    def areSentencesSimilar(sentence1: String, sentence2: String): Boolean =
      @annotation.tailrec
      def walkThrough(
        isFirst: Boolean,
        isFillingGap: Boolean,
        mandatory: List[String],
        wordsToMatch: List[String],
      ): Boolean =
        if mandatory.isEmpty then true
        else if mandatory.sizeIs == wordsToMatch.size then
          mandatory == wordsToMatch
        else if mandatory.sizeIs > wordsToMatch.size then false
        else if mandatory.head == wordsToMatch.head && isFillingGap then
          mandatory == wordsToMatch
        else if mandatory.head == wordsToMatch.head then
          walkThrough(false, false, mandatory.tail, wordsToMatch.tail)
        else
          walkThrough(false, true, mandatory, wordsToMatch.tail)

      val s1Split = sentence1.split(' ').toList
      val s2Split = sentence2.split(' ').toList
      if sentence2.sizeIs > sentence1.size then
        walkThrough(true, false, s1Split, s2Split) || walkThrough(true, false, s1Split.reverse, s2Split.reverse)
      else
        walkThrough(true, false, s2Split, s1Split) || walkThrough(true, false, s2Split.reverse, s1Split.reverse)

object SentenceSimilarityApp extends App:

  {
    val sentence1 = "My name is Haley"
    val sentence2 = "My Haley"
    val res = SentenceSimilarity.areSentencesSimilar(sentence1, sentence2)
    assert(res)
  }
  {
    val sentence1 = "a lot of words"
    val sentence2 = "of"
    val res = SentenceSimilarity.areSentencesSimilar(sentence1, sentence2)
    assert(!res)
  }
  {
    val sentence1 = "Eating right now"
    val sentence2 = "Eating"
    val res = SentenceSimilarity.areSentencesSimilar(sentence1, sentence2)
    assert(res)
  }
  {
    val sentence1 = "CwFfRo regR"
    val sentence2 = "CwFfRo H regR"
    val res = SentenceSimilarity.areSentencesSimilar(sentence1, sentence2)
    assert(res)
  }
  {
    val sentence1 = "eTUny i b R UFKQJ EZx JBJ Q xXz"
    val sentence2 = "eTUny i R EZx JBJ xXz"
    val res = SentenceSimilarity.areSentencesSimilar(sentence1, sentence2)
    assert(!res)
  }
  {
    val sentence1 = "xD iP tqchblXgqvNVdi"
    val sentence2 = "FmtdCzv Gp YZf UYJ xD iP tqchblXgqvNVdi"
    val res = SentenceSimilarity.areSentencesSimilar(sentence1, sentence2)
    assert(res)
  }
  {
    val sentence1 = "A"
    val sentence2 = "a A b A"
    val res = SentenceSimilarity.areSentencesSimilar(sentence1, sentence2)
    assert(res)
  }
