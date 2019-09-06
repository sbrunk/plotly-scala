package codegen
import scala.io.Source
import scala.util.Try
import Attribute._
import org.scalafmt.Scalafmt
import pprint.PPrinter
import JsonCodec._
import io.circe.parser.parse

import scala.meta._
import scala.collection.immutable
import scala.io.Source
import _root_.io.circe._
import os.Path

import scala.util.Try

object Main {

  def transform(name: String, trace: Trace): Tree = {
    val params = trace.attributes.map {
      case (paramName, tpe) =>
        param"${Term.Name(paramName)}: ${attributeType(Some(name), paramName, tpe)}"
    }.toList
    val cls = q"case class ${Type.Name(name.capitalize)}(..$params)"
    // for each nested type like ObjectT/EnumeratedT, we also have to create the corresponding definitions
    val companion = trace.attributes
      .collect {
        case(name, tpe: ObjectT) => transform(name, tpe)
        case(name, tpe: EnumeratedT) => transform(name, tpe)
      }
      .toList
      .flatten
    val layoutParams = trace.layoutAttributes.map {
      case (paramName, tpe) =>
        param"${Term.Name(paramName)}: ${attributeType(Some("Layout"), paramName, tpe)}"
    }.toList
    val layoutCls = q"case class Layout(..$layoutParams) extends plotly.layout.Layout"
    val layout = trace.layoutAttributes
      .collect {
        case(name, tpe: ObjectT) => transform(name, tpe)
        case(name, tpe: EnumeratedT) => transform(name, tpe)
      }
      .toList
      .flatten
    q"""
       package traces {
       import enumeratum._
         $cls
         object ${Term.Name(name.capitalize)} {
           ..$companion
         }
         object Layout {
           $layoutCls
           ..$layout
         }
      }
      """
  }

  def transform(name: String, obj: ObjectT): Seq[Stat] = {
    val params = obj.items.map {
      case (paramName, tpe) =>
        param"${Term.Name(paramName)}: ${attributeType(Some(name), paramName, tpe)}"
    }.toList
    val nestedObjects = obj.items
      .collect {
        case(name, tpe: ObjectT) => transform(name, tpe)
        case(name, tpe: EnumeratedT) => transform(name, tpe)
      }
      .toList
      .flatten
    val companion =
      if (nestedObjects.nonEmpty)
        Option(q"""
        object ${Term.Name(name.capitalize)} {
         ..$nestedObjects
        }
       """)
      else None
    val cls = q"case class ${Type.Name(name.capitalize)}(..$params)"
    Seq(cls) ++ companion
  }

  def transform(name: String, enum: EnumeratedT): Seq[Stat] = {
    //println(s"enum: $name, $enum")
    val enumType = Type.Name(name.capitalize)
    val enumObject = Term.Name(name.capitalize)
    val t = init"${enumType}()"
    def enumValue(name: String) = q"""case object ${Term.Name(name.capitalize)} extends $enumType()"""
    val enumValues = enum.values
      .map(x => if(x == "") "empty" else x)
      .map(enumValue).toList
    Seq(q"""
      sealed trait $enumType extends EnumEntry""",
      q"""object $enumObject extends Enum[$enumType] {
        val values = findValues
       ..$enumValues
      }
    """)
  }

  def attributeType(
      parent: Option[String],
      name: String,
      attribute: Attribute): Type = {
    val x = attribute match {
      case a: DataArrayT => t"plotly.Sequence"
      case a: EnumeratedT =>
        parent
          .map(parent =>
            Type.Select(Term.Name(parent.capitalize), Type.Name(name.capitalize)))
          .getOrElse(Type.Name(name.capitalize))
      case a: BooleanT => t"Boolean"
      case a: NumberT => t"Double"
      case a: IntegerT => t"Integer"
      case a: StringT => t"String"
      case a: ColorT => t"plotly.element.Color"
      case a: ColorListT => t"Array[plotly.element.Color]"
      case a: ColorScaleT => t"TODOColorScaleT"
      case a: AngleT => t"Integer" //A number (in degree) between -180 and 180.
      case a: SubPlotIdT => t"String" //An id string of a subplot type (given by dflt), optionally followed by an integer >1. e.g. if dflt='geo', we can have 'geo', 'geo2', 'geo3', ...
      case a: FlaglistT => t"String" // A string representing a combination of flags (order does not matter here). Combine any of the available `flags` with *+*. (e.g. ('lines+markers')). Values in `extras` cannot be combined.
      case a: AnyT => Type.Name("TODO AnyT")
      case a: InfoArrayT => Type.Name("TODO InfoArrayT")
      case a: ObjectT =>
        parent
          .map(parent =>
            Type.Select(Term.Name(parent.capitalize), Type.Name(name.capitalize)))
          .getOrElse(Type.Name(name.capitalize))
    }
    x
  }

  def transform(layout: Layout): Tree = {
    val params = layout.layoutAttributes.map {
      case (paramName, tpe) =>
        param"${Term.Name(paramName)}: ${attributeType(Some("Layout"), paramName, tpe)}"
    }.toList
    val cls = q"abstract class Layout(..$params)"
    // for each nested type like ObjectT/EnumeratedT, we also have to create the corresponding definitions
    val companion = layout.layoutAttributes
      .collect {
        case(name, tpe: ObjectT) => transform(name, tpe)
        case(name, tpe: EnumeratedT) => transform(name, tpe)
      }
      .toList
      .flatten
    q"""
       package layout {
       import enumeratum._
         $cls
         object Layout {
           ..$companion
         }
      }
      """
  }

  def main(args: Array[String]): Unit = plotlySchemaToScala(new java.io.File(args.head))

  def plotlySchemaToScala(base: java.io.File): Seq[java.io.File] = {

    val plotlySchemaJson = Source.fromFile("plotschema.json").mkString

    val decodedSchema: Either[Error, PlotlySchema] = for {
      json   <- parse(plotlySchemaJson)
      schema <- json.hcursor.downField("schema").as[PlotlySchema]
    } yield schema

    val sources = decodedSchema.map { schema =>
      schema.traces.map {
        case (name, trace) =>
          (name, Scalafmt.format(transform(name, trace).toString()).get)
      } + ("Layout" -> Scalafmt.format(transform(schema.layout).toString()).get)
    }.toTry.get

    sources.map {
      case(name, source) =>
        val outFile = Path(base) / s"${name.capitalize}.scala"
        println(outFile)
        os.write.over(outFile, source, createFolders = true)
        outFile.toIO
    }.toSeq
  }

}