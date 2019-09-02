package codegen

import Attribute._
import io.circe.Decoder.Result
import io.circe._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveDecoder
import io.circe.generic.extras.auto._
import io.circe.Decoder.Result
import io.circe.optics.JsonPath._
import io.circe._
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._
import io.circe.generic.extras.auto._
import cats.syntax.either._
import io.circe.Decoder.Result
import io.circe.optics.JsonPath._
import cats.syntax.functor._
import io.circe.generic.extras.AutoDerivation

import cats.data.StateT
import cats.instances.either._
import io.circe.{ ACursor, Decoder, Json }

object JsonCodec extends AutoDerivation {
  // KebabCase but with _ instead of -
  val caseTransformation: String => String = _.replaceAll(
    "([A-Z]+)([A-Z][a-z])",
    "$1_$2"
  ).replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase

  val removeT: String => String = _.init

  val metaKeys = Set("_isSubplotObj", "_isLinkedToArray", "_arrayAttrRegexps", "_deprecated", "description", "role",
    "editType", "impliedEdits")

  implicit val genDevConfig: Configuration =
    Configuration.default
      .withDefaults
      .withDiscriminator("valType")
      .copy(transformConstructorNames = removeT andThen caseTransformation)

  implicit val decodeLayout: Decoder[Layout] =
    Decoder.fromState(
      for {
        _ <- Decoder.state.decodeField[String]("editType") // ignore
        attributes <- StateT.inspectF((_: ACursor).as[Map[String, Attribute]])
      } yield Layout(attributes)
    ).prepare(_.downField("layoutAttributes"))

  implicit val traceDecoder: Decoder[Trace] = new Decoder[Trace] {
    override def apply(c: HCursor): Result[Trace] = {
      for {
        meta <- c.downField("meta").as[Meta]
        attributesCursor = c.downField("attributes").withFocus(json =>
          json.withObject(_.filterKeys(key => !metaKeys.contains(key) && key != "type").asJson))
        attributes <- attributesCursor.as[Map[String, Attribute]]
        layoutAttributes <- c.getOrElse[Map[String, Attribute]]("layoutAttributes")(Map.empty)
      } yield Trace(meta, attributes, layoutAttributes)
    }
  }
  implicit val decodeMeta: Decoder[Meta] = deriveDecoder
  implicit val decodeEnumeratedT: Decoder[EnumeratedT] =  {
    implicit val intStringDecoder: Decoder[String] =
      Decoder.decodeString
        .or(Decoder.decodeBoolean.map(_.toString))
        .or(Decoder.decodeInt.map(_.toString))
    deriveDecoder
  }
  implicit val attributeDecoder: Decoder[Attribute] = new Decoder[Attribute] {

    // if valType is set, try to decode the corresponding type
    override def apply(c: HCursor): Result[Attribute] = {

      def tryValType = c.get[String]("valType")
        .map { valType => {
          val d: Decoder[Attribute] = valType match {
            case "data_array" => Decoder[DataArrayT].widen
            case "enumerated" => Decoder[EnumeratedT].widen
            case "boolean" => Decoder[BooleanT].widen
            case "number" | "angle" => Decoder[NumberT].widen
            case "integer" => Decoder[IntegerT].widen
            case "string" => Decoder[StringT].widen
            case "color" => Decoder[ColorT].widen
            case "colorlist" => Decoder[ColorListT].widen
            case "colorscale" => Decoder[ColorScaleT].widen
            case "subplotid" => Decoder[SubPlotIdT].widen
            case "flaglist" => Decoder[FlaglistT].widen
            case "any" => Decoder[AnyT].widen
            case "info_array" => Decoder[InfoArrayT].widen
            case unknown => Decoder.failedWithMessage[Attribute]("unknown valType: " + unknown)
          }
          d.apply(c)
        }
        }

      def tryRole = c.get[String]("role").flatMap {
        case "object" => {
          for {
            obj <- c.as[JsonObject]
            meta = obj.filterKeys(metaKeys.contains).toMap
            objectT <- c.downField("items").as[Map[String, Attribute]].map(
              items => ObjectT(items, meta, nestedItems = true
              )).orElse {
              val attr = obj.filterKeys(key => !metaKeys.contains(key))
              Json.fromJsonObject(attr).as[Map[String, Attribute]].map(
                items => ObjectT(items, meta) //deprecated = deprecated // TODO
              )
            }
          } yield objectT: Attribute
        }
        case unknown => Left(DecodingFailure("unknown role: " + unknown, c.history))
      }

      tryValType.getOrElse(tryRole)
    }

    //      override def apply(c: HCursor): Result[EnumeratedT] =
    //        for {
    //          values <-
    //          dflt
    //          role
    //          editType
    //          description
    //        }
    //    }
    //    )

  }

  private def safeDropKeys(json: Json)(keys: String*): Json =
    json.withObject(obj => keys.foldLeft(obj)((acc, s) => acc.remove(s)).asJson)
}
