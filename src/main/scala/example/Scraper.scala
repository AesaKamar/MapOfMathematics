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
  val doc     = browser.parseFile("assets/00.html")

  val content = doc >> element("div #content")
  val entries =
    (content >> elementList("div div"))
      .filter(x => x.hasAttr("class") && x.attr("class").startsWith("indent"))
      .map { x =>
        val key = (x >> elements("strong")).map(_.text).headOption
        val indentLevel =
          (x >> elements("div")).map(_.attr("class")).map(_.lastOption.map(_.asDigit)).headOption.flatten
        val description =
          x.childNodes
            .filter(_.isInstanceOf[TextNode])
            .map { case TextNode(c) => c }
            .toList
            .fold("")(_.concat(_))
            .trim
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

}

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
