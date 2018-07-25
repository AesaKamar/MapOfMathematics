package example

import cats._
import cats.data._
import cats.effect._
import fastparse.core.Parsed.{Failure, Success}
import java.util.Random

object Scraper {
  import net.ruippeixotog.scalascraper.browser.JsoupBrowser
  import net.ruippeixotog.scalascraper.dsl.DSL._
  import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
  import net.ruippeixotog.scalascraper.model._

  private val browser = JsoupBrowser()
  def parseFile(fileName: String) = {
    val doc = browser.parseFile(fileName)

    val content = doc >> element("div #content")

    val entries =
      (content >> elementList("div div"))
        .filter(x => x.hasAttr("class") && x.attr("class").startsWith("indent"))
        .map { x =>
          val key = (x >> elements("strong")).map(_.text).headOption
          val indentLevel =
            (x >> elements("div")).map(_.attr("class")).map(_.lastOption.map(_.asDigit)).headOption.flatten
          val description = {
            val latexedDescriptions = x.childNodes
              .map {
                case TextNode(" ") => None
                case TextNode(c)   => Some(NormalDescription(c))
                case ElementNode(e) if (e >> elements("script")).nonEmpty => {
                  val r = (e >> elements("script")).map(_.innerHtml)
                  r.headOption.map(MathTexDescription)
                }
                case _ => None
              }
              .toList
              .flatten
            resolveDescription(latexedDescriptions)
          }

          val links = (x >> elementList("a"))
            .map(_.text.trim)

          (indentLevel, key, description, links)
        }

    val parsedEntries = entries
      .map {
        case (indentLevel, key, description, links) =>
          import Parser._
          val identifier = key
            .map(x => idParser.parse(x))
            .flatMap {
              case Success(v, _)    => Some(v)
              case Failure(_, _, _) => None
            }
          val parsedLinks = links
            .map(x => idParser.parse(x))
            .flatMap {
              case Success(v, _)    => Some(v)
              case Failure(_, _, _) => None
            }

          (indentLevel, identifier, description, parsedLinks)
      }
      .filter(_._2.nonEmpty)

    parsedEntries
  }

  def resolveDescription(descriptions: List[Description]): NormalDescription = {
    import com.github.tomtung.latex2unicode._
    val newDescs = descriptions.map {
      case d: NormalDescription  => d
      case MathTexDescription(d) => NormalDescription(LaTeX2Unicode.convert(d))
    }
    val accumulated =
      newDescs.foldLeft(new StringBuilder) { case (acc, NormalDescription(s)) => acc.append(s) }.toString().trim

    val withoutSeeAlsoAnnotations = accumulated.replaceAll("\\[.*\\]", "")

    NormalDescription(withoutSeeAlsoAnnotations.trim)
  }

}

sealed trait Description
final case class NormalDescription(value: String)  extends Description
final case class MathTexDescription(value: String) extends Description

sealed trait Identifier
final case class Area(id: String) extends Identifier
final case class SubArea(
    areaId: String,
    subId: String)
  extends Identifier
final case class Specialization(
    areaId: String,
    subId: String,
    specId: String)
  extends Identifier

object Parser {
  import fastparse.all._

  val digit         = P(CharIn('0' to '9'))
  val capLetter     = P(CharIn('A' to 'Z'))
  val areaParser    = P(digit ~ digit ~ End).!.map(Area)
  val subAreaParser = P((digit ~ digit).! ~ capLetter.! ~ (End | "xx" ~ End)).map { case (a, s) => SubArea(a, s) }
  val specParser = P((digit ~ digit).! ~ capLetter.! ~ (digit ~ digit).! ~ End).map {
    case (a, sa, sp) => Specialization(a, sa, sp)
  }

  val idParser: Parser[Identifier] = P(specParser | subAreaParser | areaParser)

}

final case class MathSciNetEntry(
    level: Int,
    identifier: Identifier,
    description: String,
    links: List[Identifier])
object MathSciNetEntry {
  def fromTuple(t: (Option[Int], Option[Identifier], NormalDescription, List[Identifier])) = t match {
    case (Some(level), Some(id), NormalDescription(desc), links) => MathSciNetEntry(level, id, desc, links)
  }
}

object EntryOps {

  private val random = new Random(12345L)

  def genRandomDouble = (random.nextDouble() - 0.5) * 10

  def entryToSigmaNode(entry: MathSciNetEntry): SigmaNode = {
    val id     = identifierAsString(entry.identifier)
    val label  = entry.description
    val (x, y) = (0, 0)
    val size   = 3

    SigmaNode(id, label, x, y, size)
  }

  def identifierAsString(identifier: Identifier): String = {
    identifier match {
      case Area(a)                 => a
      case SubArea(a, b)           => a + b
      case Specialization(a, b, c) => a + b + c
    }
  }
  val rootNodeId = "."
  def entryToSigmaEdges(entry: MathSciNetEntry): List[SigmaEdge] = {
    val parentEdge = entry.identifier match {
      case Area(a) => {
        val source = rootNodeId
        val target = a
        SigmaEdge(s"$source->$target", source, target)
      }
      case SubArea(a, b) => {
        val source = a
        val target = a + b
        SigmaEdge(s"$source->$target", source, target)
      }
      case Specialization(a, b, c) => {
        val source = a + b
        val target = a + b + c
        SigmaEdge(s"$source->$target", source, target)
      }
    }

    val links = entry.links.map { linkTarget =>
      val source = identifierAsString(entry.identifier)
      val target = identifierAsString(linkTarget)
      SigmaEdge(s"$source->$target", source, target)
    }

    parentEdge :: links

  }

}

final case class SigmaNode(
    id: String,
    label: String,
    x: Double,
    y: Double,
    size: Double)

final case class SigmaEdge(
    id: String,
    source: String,
    target: String)
