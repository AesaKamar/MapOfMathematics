package example

import org.scalatest._
import pprint._
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._

class ScraperTest extends AsyncFreeSpec with Matchers {
  import Scraper._
  "Grabbing an html document" in {
    parseFile("assets/57.html")
      .map(Entry.fromTuple)
      .map(x => pprintln(x))
    succeed
  }
  "listing files" ignore {
    import better.files._
    import better.files.Dsl._
    import File._
    val files = ls(file"assets/")
    pprintln(files.toList.map(_.path.toString).sorted)
    succeed
  }
}
