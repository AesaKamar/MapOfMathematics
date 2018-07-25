package example

import org.scalatest._
import pprint._
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import scala.concurrent.Future

class ScraperTest extends AsyncFreeSpec with Matchers {
  import Scraper._
  import io.circe._, io.circe.generic.auto._, io.circe.syntax._

  def parseHtmlToEntryNodesAndEdges(fileName: String): (List[SigmaNode], List[SigmaEdge]) = {
    val entryNodes = parseFile(fileName)
      .map(MathSciNetEntry.fromTuple)

    val sigmaNodes = entryNodes.map(EntryOps.entryToSigmaNode)
    val sigmaEdges = entryNodes.flatMap(EntryOps.entryToSigmaEdges)

    (sigmaNodes, sigmaEdges)
  }

  "listing files" in {
    import better.files._
    import better.files.Dsl._
    import File._
    val files = ls(file"assets/")
    val paths = files.toList
      .map(_.path.toString)
      .filter(x => x.endsWith("html"))
      .sorted

    val parseResults =
      paths.map(parseHtmlToEntryNodesAndEdges).unzip match {
        case (nodes, edges) => (nodes.flatten.asJson, edges.flatten.asJson)
      }

    val (nodesJson, edgesJson) = parseResults

    file"docs/nodes.json" < nodesJson.toString
    file"docs/edges.json" < edgesJson.toString

    Future(succeed)
  }
}
