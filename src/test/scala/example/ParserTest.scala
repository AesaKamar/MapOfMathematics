package example

import org.scalatest._
import pprint._
import org.scalatest.EitherValues._

class ScraperTest extends AsyncFreeSpec with Matchers {
  import Scraper._
  "Grabbing an html document" in {
    val response = doc

    parsedEntries.map(x => pprintln(x))
    succeed
  }
}
