package example

import org.scalatest._
import pprint._
import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import scala.concurrent.Future

class ScraperTest extends AsyncFreeSpec with Matchers {
  import Scraper._
  import io.circe._, io.circe.generic.auto._, io.circe.syntax._
  import Inspectors._

  def parseHtmlToEntryNodesAndEdges(fileName: String): (List[SigmaNode], List[SigmaEdge]) = {
    println(fileName)
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

    val sigmaRoot = SigmaNode(".", "Truth", 0, 0, 3)
    val parseResults =
      paths.map(parseHtmlToEntryNodesAndEdges).unzip match {
        case (nodes, edges) => (nodes.flatten.toSet.+(sigmaRoot), edges.flatten.toSet)
      }
    val (nodes, edges) = parseResults

    val missingSources = edges
      .flatMap(e => nodes.filterNot(_.id == e.source).headOption.map(_ => e))
    val missingTargets = edges
      .flatMap(e => nodes.filterNot(_.id == e.target).headOption.map(_ => e))

    val edgesWithSourceAndTarget = edges -- (missingSources ++ missingTargets)

    file"docs/nodes.json" < nodes.asJson.toString
    file"docs/edges.json" < edgesWithSourceAndTarget.asJson.toString

    Future(succeed)
  }
}
