/**
  * Created by mmgreiner on 01.12.16.
  * @author mmgreiner
  *
  */

/**
  * Class to hold options typically used for tokenizing and streaming
  * @param numbers
  * @param stopWords
  * @param stemming
  * @param chopping
  * @param maxDocs
  */
case class TipsterOptions(numbers: Boolean = true,
                          trimming: Boolean = true,
                          stopWords: Boolean = true,
                          stemming: Boolean = true,
                          chopping: Integer = -1,
                          useSynonyms: Boolean = true,
                          maxDocs: Int = Int.MaxValue,
                          ngramSize: Int = 0,
                          splitLong: Boolean = true)

