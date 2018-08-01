package example

import cats._, data._, implicits._, syntax._
import java.util.Random

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

    val tightness = 3

    val posX = (Math.cos(radsToMovePerGroup * group) * (level / tightness)) + (genRandomDouble / math.log(
      (level / tightness) * 5))
    val posY = (Math.sin(radsToMovePerGroup * group) * (level / tightness)) + (genRandomDouble / math.log(
      (level / tightness) * 5))

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
    val color = entry.level match {
      case 1 => "#1e1e1e"
      case 2 => "#1e1e1e"
      case 3 => "#1e1e1e"
    }

    SigmaNode(id, label, x, y, size, color.some)
  }

  val rootNodeId = "."
  def entryToSigmaEdges(entry: MathSciNetEntry): List[SigmaEdge] = {
    val parentEdge = entry.identifier match {
      case Area(a) => {
        val source = rootNodeId
        val target = a
        SigmaEdge(s"$source->$target", source, target, 0.25, 1, "#a3a3a3".some)
        None
      }
      case SubArea(a, b) => {
        val source = a
        val target = a + b
        SigmaEdge(s"$source->$target", source, target, 10, 1, "#a3a3a3".some).some
      }
      case Specialization(a, b, c) => {
        val source = a + b
        val target = a + b + c
        SigmaEdge(s"$source->$target", source, target, 2, 1, "#a3a3a3".some).some
      }
    }

    val links = entry.links.map { linkTarget =>
      val source = entry.identifier.asString
      val target = linkTarget.asString
      SigmaEdge(s"$source->$target", source, target, 5, 1, "#dbdbdb".some)
    }
    //    val links = List.empty

    parentEdge.toList ++ links

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
    weight: Double,
    size: Double,
    color: Option[String] = None)
