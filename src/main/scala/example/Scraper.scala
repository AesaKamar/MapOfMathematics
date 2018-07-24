package example

import cats._
import cats.data._
import cats.effect._
import fastparse.core.Parsed.{Failure, Success}

object Scraper {
  import net.ruippeixotog.scalascraper.browser.JsoupBrowser
  import net.ruippeixotog.scalascraper.dsl.DSL._
  import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
  import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
  import net.ruippeixotog.scalascraper.model._

  val browser = JsoupBrowser()
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
  val subAreaParser = P((digit ~ digit).! ~ capLetter.! ~ End).map { case (a, s) => SubArea(a, s) }
  val specParser = P((digit ~ digit).! ~ capLetter.! ~ (digit ~ digit).! ~ End).map {
    case (a, sa, sp) => Specialization(a, sa, sp)
  }

  val idParser: Parser[Identifier] = P(specParser | subAreaParser | areaParser)

}

final case class Entry(
    level: Int,
    identifier: Identifier,
    description: String,
    links: List[Identifier])
object Entry {
  def fromTuple(t: (Option[Int], Option[Identifier], NormalDescription, List[Identifier])) = t match {
    case (Some(level), Some(id), NormalDescription(desc), links) => Entry(level, id, desc, links)
  }
}
