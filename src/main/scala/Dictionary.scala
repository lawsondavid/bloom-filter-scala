import scala.io.{BufferedSource, Source}

/**
 * Home - Created by dave on 27/09/14.
 */
object Dictionary {
  def wordCount = getWords.size

  def processEntries(process: (String) => (Unit)) = getWords.foreach(process)

  private def getWords = Source.fromFile("/usr/share/dict/words").getLines()
}
