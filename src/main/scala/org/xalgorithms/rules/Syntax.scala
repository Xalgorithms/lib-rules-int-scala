// Copyright (C) 2018 Don Kelly <karfai@gmail.com>
// Copyright (C) 2018 Hayk Pilosyan <hayk.pilos@gmail.com>

// This file is part of Interlibr, a functional component of an
// Internet of Rules (IoR).

// ACKNOWLEDGEMENTS
// Funds: Xalgorithms Foundation
// Collaborators: Don Kelly, Joseph Potvin and Bill Olders.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
package org.xalgorithms.rules

import org.bson.BsonDocument
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.io.Source

import org.xalgorithms.rules.steps._
import org.xalgorithms.rules.elements._

object StepProduce {
  implicit val columnReads: Reads[Column] = (
    (JsPath \ "table").read[JsObject] and
    (JsPath \ "sources").read[JsArray]
  )(produce_column _)

  implicit val tableReferenceReads: Reads[TableReference] = (
    (JsPath \ "section").read[String] and
    (JsPath \ "key").read[String]
  )(produce_table_reference _)

  implicit val colsSourceReads: Reads[ColumnsTableSource] = (
    (JsPath \ "columns").read[JsArray] and
    (JsPath \ "whens").readNullable[JsArray]
  )(produce_columns_table_source _)

  implicit val colSourceReads: Reads[ColumnTableSource] = (
    (JsPath \ "name").read[String] and
    (JsPath \ "source").read[String] and
    (JsPath \ "whens").readNullable[JsArray]
  )(produce_column_table_source _)

  implicit val whenReads: Reads[When] = (
    (JsPath \ "left").read[JsObject] and
    (JsPath \ "right").read[JsObject] and
    (JsPath \ "op").read[String]
  )(produce_when _)

  implicit val valueReads: Reads[Value] = (
    (JsPath \ "type").read[String] and
    (JsPath).read[JsObject]
  )(produce_value _)

  implicit val assignmentReads: Reads[Assignment] = (
    (JsPath \ "target").read[String] and
    (JsPath \ "source").read[JsObject]
  )(produce_assignment _)

  implicit val revisionReads : Reads[RevisionSource] = (
    (JsPath \ "op").read[String] and
    (JsPath \ "source").read[JsObject]
  )(produce_revision_source _)

  implicit val addRevisionSourceReads : Reads[AddRevisionSource] = (
    (JsPath \ "column").read[String] and
    (JsPath \ "table").read[JsObject] and
    (JsPath \ "whens").read[JsArray]
  )(produce_add_revision_source _)
  
  implicit val updateRevisionSourceReads : Reads[UpdateRevisionSource] = (
    (JsPath \ "column").read[String] and
    (JsPath \ "table").read[JsObject] and
    (JsPath \ "whens").read[JsArray]
  )(produce_update_revision_source _)

  implicit val deleteRevisionSourceReads : Reads[DeleteRevisionSource] = (
    (JsPath \ "column").read[String] and
    (JsPath \ "whens").read[JsArray]
  )(produce_delete_revision_source _)

  def stringOrNull(content: JsObject, k: String): String = {
    return (content \ k).validate[String].getOrElse(null)
  }

  def doubleOrNull(content: JsObject, k: String): Double = {
    var rv = null.asInstanceOf[Double]
    val sv = stringOrNull(content, k)

    if (null != sv) {
      rv = sv.toDouble
    }

    return rv
  }

  def produce_packaged_table_reference(content: JsObject): PackagedTableReference = {
    return new PackagedTableReference(
      stringOrNull(content, "package"),
      stringOrNull(content, "id"),
      stringOrNull(content, "version"),
      stringOrNull(content, "name")
    )
  }

  def produce_optional_whens(whens_opt: Option[JsArray]): Seq[When] = whens_opt match {
    case Some(whens) => whens.validate[Seq[When]].getOrElse(Seq())
    case None => Seq()
  }

  def produce_columns_table_source(
    columns: JsArray, whens_opt: Option[JsArray]): ColumnsTableSource = {
    return new ColumnsTableSource(
      columns.validate[Seq[String]].getOrElse(Seq()),
      produce_optional_whens(whens_opt))
  }

  def produce_column_table_source(
    name: String, source: String, whens_opt: Option[JsArray]): ColumnTableSource = {
    new ColumnTableSource(name, source, produce_optional_whens(whens_opt))
  }

  def produce_when(left: JsObject, right: JsObject, op: String): When = {
    return new When(
      left.validate[Value].getOrElse(null),
      right.validate[Value].getOrElse(null),
      op)
  }

  def produce_value(vt: String, content: JsObject): Value = {
    if ("string" == vt) {
      return new StringValue(stringOrNull(content, "value"))
    } else if ("number" == vt) {
      return new NumberValue(doubleOrNull(content, "value"))
    } else if ("reference" == vt) {
      return new DocumentReferenceValue(stringOrNull(content, "section"), stringOrNull(content, "key"))
    } else if ("function" == vt) {
      return new FunctionValue(
        stringOrNull(content, "name"),
        (content \ "args").validate[Seq[Value]].getOrElse(Seq())
      )
    }

    return null
  }

  def produce_assignment(target: String, source: JsObject): Assignment = {
    return new Assignment(
      target, source.validate[Value].getOrElse(null)
    )
  }

  def produce_revision_source(op: String, source: JsObject): RevisionSource = op match {
    case "add" => source.validate[AddRevisionSource].getOrElse(null)
    case "update" => source.validate[UpdateRevisionSource].getOrElse(null)
    case "delete" => source.validate[DeleteRevisionSource].getOrElse(null)
    case _ => null
  }

  def produce_add_revision_source(column: String, table: JsObject, whens: JsArray): AddRevisionSource = {
    return new AddRevisionSource(
      column,
      whens.validate[Seq[When]].getOrElse(Seq()),
      table.validate[TableReference].getOrElse(null)
    )
  }

  def produce_update_revision_source(column: String, table: JsObject, whens: JsArray): UpdateRevisionSource = {
    return new UpdateRevisionSource(
      column,
      whens.validate[Seq[When]].getOrElse(Seq()),
      table.validate[TableReference].getOrElse(null)
    )
  }

  def produce_delete_revision_source(column: String, whens: JsArray): DeleteRevisionSource = {
    return new DeleteRevisionSource(
      column,
      whens.validate[Seq[When]].getOrElse(Seq())
    )
  }

  def produce_column(table_reference: JsObject, sources: JsArray): Column = {
    return new Column(
      table_reference.validate[TableReference].getOrElse(null),
      sources.validate[Seq[ColumnsTableSource]].getOrElse(Seq()) ++
        sources.validate[Seq[ColumnTableSource]].getOrElse(Seq())
    )
  }

  def produce_table_reference(section: String, key: String): TableReference = {
    return new TableReference(section, key)
  }

  def produce_assemble(content: JsObject): Step = {
    return new AssembleStep(
      stringOrNull(content, "table_name"),
      (content \ "columns").validate[Seq[Column]].getOrElse(null)
    )
  }

  def produce_filter(content: JsObject): Step = {
    return new FilterStep(
      (content \ "table").validate[TableReference].getOrElse(null),
      (content \ "filters").validate[Seq[When]].getOrElse(Seq())
    )
  }

  def produce_keep(content: JsObject): Step = {
    return new KeepStep(stringOrNull(content, "name"), stringOrNull(content, "table_name"))
  }

  def produce_map(content: JsObject): Step = {
    return new MapStep(
      (content \ "table").validate[TableReference].getOrElse(null),
      (content \ "assignments").validate[Seq[Assignment]].getOrElse(Seq())
    )
  }

  def produce_reduce(content: JsObject): Step = {
    return new ReduceStep(
      (content \ "filters").validate[Seq[When]].getOrElse(Seq()),
      (content \ "table").validate[TableReference].getOrElse(null),
      (content \ "assignments").validate[Seq[Assignment]].getOrElse(Seq())
    )
  }

  def produce_require(content: JsObject): Step = {
    return new RequireStep(
      produce_packaged_table_reference((content \ "reference").as[JsObject]),
      (content \ "indexes").as[Seq[String]])
  }

  def produce_revise(content: JsObject): Step = {
    return new ReviseStep(
      (content \ "table").validate[TableReference].getOrElse(null),
      (content \ "revisions").validate[Seq[RevisionSource]].getOrElse(Seq())
    )
  }

  val fns = Map[String, (JsObject) => Step](
    "assemble" -> produce_assemble,
    "filter" -> produce_filter,
    "keep" -> produce_keep,
    "map" -> produce_map,
    "reduce" -> produce_reduce,
    "require" -> produce_require,
    "revise" -> produce_revise
  )

  def apply(name: String, content: JsObject): Step = {
    return fns(name)(content)
  }
}

object SyntaxFromRaw {
  implicit val stepReads: Reads[Step] = (
    (JsPath \ "name").read[String] and
    (JsPath).read[JsObject]
  )(StepProduce.apply _)

  def apply(s: String): Seq[Step] = {
    val res = (Json.parse(s) \ "steps").validate[Seq[Step]]
    res.getOrElse(Seq())
  }
}

object SyntaxFromSource {
  def apply(source: Source): Seq[Step] = {
    SyntaxFromRaw(source.mkString)
  }
}

object SyntaxFromBson {
  def apply(doc: BsonDocument): Seq[Step] = {
    // cheat for now and pull out the raw json
    SyntaxFromRaw(doc.toJson)
  }
}
