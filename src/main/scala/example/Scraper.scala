package example

import cats._
import cats.data._
import cats.effect._
import cats.implicits._
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
                case ElementNode(e) if (e >> elements("script")).nonEmpty =>
                  val r = (e >> elements("script")).map(_.innerHtml)
                  r.headOption.map(MathTexDescription)
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
object Identifier {
  implicit class IdentifierHasGroup(identifier: Identifier) {
    def group = identifier match {
      case Area(id)                              => id.toInt
      case SubArea(areaId, subId)                => areaId.toInt
      case Specialization(areaId, subId, specId) => areaId.toInt
    }
  }

  implicit class IdentifierHasString(identifier: Identifier) {
    def asString: String = {
      identifier match {
        case Area(a)                 => a
        case SubArea(a, b)           => a + b
        case Specialization(a, b, c) => a + b + c
      }
    }
  }

}

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

  def genRandomDouble = (random.nextDouble() - 0.5)

  def placeParticle(
      level: Int,
      group: Int
    ): (Double, Double) = {
    val totalNumberOfGroups   = 100.0
    val degreesToMovePerGroup = 360.0 / totalNumberOfGroups
    val radsToMovePerGroup    = Math.toRadians(degreesToMovePerGroup)

//    val radius = level * 10

    val posX = (Math.cos(radsToMovePerGroup * group) * level) + (genRandomDouble / math.log(level * 5))
    val posY = (Math.sin(radsToMovePerGroup * group) * level) + (genRandomDouble / math.log(level * 5))

    (posX, posY)
  }

  def entryToSigmaNode(entry: MathSciNetEntry): SigmaNode = {
    val id     = entry.identifier.asString
    val label  = entry.description
    val (x, y) = placeParticle(entry.level, entry.identifier.group)
    val size = entry.level match {
      case 1 => 6
      case 2 => 4
      case 3 => 2
    }

    SigmaNode(id, label, x, y, size, "#1e1e1e".some)
  }

  val rootNodeId = "."
  def entryToSigmaEdges(entry: MathSciNetEntry): List[SigmaEdge] = {
    val parentEdge = entry.identifier match {
      case Area(a) => {
        val source = rootNodeId
        val target = a
        SigmaEdge(s"$source->$target", source, target, 1, "#a3a3a3".some)
      }
      case SubArea(a, b) => {
        val source = a
        val target = a + b
        SigmaEdge(s"$source->$target", source, target, 0.4, "#a3a3a3".some)
      }
      case Specialization(a, b, c) => {
        val source = a + b
        val target = a + b + c
        SigmaEdge(s"$source->$target", source, target, 0.2, "#a3a3a3".some)
      }
    }

//    val links = entry.links.map { linkTarget =>
//      val source = entry.identifier.asString
//      val target = linkTarget.asString
//      SigmaEdge(s"$source->$target", source, target, 0.2)
//    }
    val links = List.empty

    parentEdge :: links

  }

}

final case class SigmaNode(
    id: String,
    label: String,
    x: Double,
    y: Double,
    size: Double,
    color: Option[String] = None)

final case class SigmaEdge(
    id: String,
    source: String,
    target: String,
    size: Double,
    color: Option[String] = None)
