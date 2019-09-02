package codegen
import io.circe.Json

case class PlotlySchema(
  //defs: Json,
  traces: Map[String, Trace],
  layout: Layout
  //transforms: Json,
  //frames: Json,
  //animation: Json
)

case class Trace(
  meta: Meta,
  attributes: Map[String, Attribute],
  layoutAttributes: Map[String, Attribute]
)

case class Meta(
  description: Option[String] = None
)


sealed trait Attribute

object Attribute {

  //case class `type`(t: String) extends Attribute

  case class DataArrayT(
    dflt: Option[Boolean] = None,
    role: Option[String] = None,
    editType: Option[String] = None,
    description: Option[String] = None
  ) extends Attribute

  case class EnumeratedT(
    values: Seq[String], // TODO decode properly
    dflt: Option[Json] = None, // TODO decode properly
    role: Option[String] = None,
    editType: Option[String] = None,
    description: Option[String] = None
  ) extends Attribute

  case class BooleanT(
    dflt: Option[Boolean] = None,
    role: Option[String] = None,
    editType: Option[String] = None,
    description: Option[String] = None
  ) extends Attribute

  case class NumberT(
    dflt: Option[Double] = None,
    min: Option[Double] = None,
    max: Option[Double] = None,
    arrayOk: Option[Boolean] = None
  ) extends Attribute

  case class IntegerT(
    dflt: Option[Int] = None,
    min: Option[Int] = None,
    max: Option[Int] = None,
    arrayOk: Option[Boolean] = None
  ) extends Attribute

  case class StringT(
    dflt: Option[Double] = None,
    role: Option[String] = None,
    editType: Option[String] = None,
    description: Option[String] = None,
    noBlank: Option[Boolean] = None,
    strict: Option[Boolean] = None,
    arrayOk: Option[Boolean] = None
  ) extends Attribute

  case class ColorT(
    role: Option[String] = None,
    arrayOk: Option[Boolean] = None,
    editType: Option[String] = None,
    description: Option[String] = None
  ) extends Attribute

  case class ColorListT(

  ) extends Attribute

  case class ColorScaleT(

  ) extends Attribute

  case class AngleT(

  ) extends Attribute

  case class SubPlotIdT(

  ) extends Attribute

  case class FlaglistT(
    flags: Json, // TODO decode properly
    dflt: Option[Json] = None, // TODO
    role: Option[String] = None,
    editType: Option[String] = None,
    description: Option[String] = None,
    extras: Option[Json] = None, // TODO
    array0k: Option[Boolean] = None
  ) extends Attribute

  case class AnyT(

  ) extends Attribute

  case class InfoArrayT(

  ) extends Attribute

  case class ObjectT(
    items: Map[String, Attribute],
    meta: Map[String, Json],
    deprecated: Map[String, Attribute] = Map.empty,
    nestedItems: Boolean = false
  ) extends Attribute

}

case class Layout(
  layoutAttributes: Map[String, Attribute]
)