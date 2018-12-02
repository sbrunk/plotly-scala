
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

  def transform(traces: Traces): Seq[Tree] = {
    traces.traces.map { case (name, trace) => transform(name, trace) }
  }.toSeq

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
    q"""
       package foo {
       import enumeratum._
        $cls
        object ${Term.Name(name.capitalize)} {
        ..$companion
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
      //.map("_" + _)
      .map(x => if(x == "") "empty" else x)
      .map(x => if(Try(Integer.parseInt(x)).isSuccess) s"_$x" else x)
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
      attribute: Attribute): scala.meta.Type = {
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

  def plotlySchemaToScala(base: java.io.File): Seq[java.io.File] = {

    val plotlySchemaJson = Source.fromFile("plotschema.json").mkString

    val decodedSchema: Either[Error, Traces] = for {
      json   <- parse(plotlySchemaJson)
      traces <- json.hcursor.downField("schema").as[Traces]
    } yield traces

    val sources = decodedSchema.map { traces =>
      traces.traces.map {
        case (name, tree) =>
          (name, Scalafmt.format(transform(name, tree).toString()).get)
      }
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