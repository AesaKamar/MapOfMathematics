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
  def distinctBy[L, E](list: List[L])(f: L => E): List[L] =
    list
      .foldLeft((Vector.empty[L], Set.empty[E])) {
        case ((acc, set), item) =>
          val key = f(item)
          if (set.contains(key)) (acc, set)
          else (acc :+ item, set + key)
      }
      ._1
      .toList

  "listing files" in {
    import better.files._
    import better.files.Dsl._
    import File._
    val files = ls(file"assets/")
    val paths = files.toList
      .map(_.path.toString)
      .filter(x => x.endsWith("html"))
      .sorted
//      .take(1)

    val sigmaRoot = SigmaNode(".", "Truth", 0, 0, 1)
    val parseResults =
      paths.map(parseHtmlToEntryNodesAndEdges).unzip match {
        case (nodesi, edgesi) => (distinctBy(nodesi.flatten)(_.id).toSet + sigmaRoot, edgesi.flatten.toSet)
      }
    val (nodes, edges) = parseResults

    val missingSources = edges
      .flatMap(e => if (!nodes.exists(_.id == e.source)) { Some(e) } else { None })
    val missingTargets = edges
      .flatMap(e => if (!nodes.exists(_.id == e.target)) { Some(e) } else { None })

    val edgesWithSourceAndTarget = edges -- (missingSources ++ missingTargets)

    file"docs/nodes.json" < nodes.asJson.toString
    file"docs/edges.json" < edgesWithSourceAndTarget.asJson.toString

    Future(succeed)
  }
}
