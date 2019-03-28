// Copyright (C) 2018 Don Kelly <karfai@gmail.com>
// Copyright (C) 2018 Hayk Pilosyan <hayk.pilos@gmail.com>

// This file is part of Interlibr, a functional component of an
// Internet of Rules (IoR).

// ACKNOWLEDGEMENTS
// Funds: Xalgorithms Foundation
// Collaborators: Don Kelly, Joseph Potvin and Bill Olders.

// This program is free software: you can redistribute it and/or
// modify it under the terms of the GNU Affero General Public License
// as published by the Free Software Foundation, either version 3 of
// the License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public
// License along with this program. If not, see
// <http://www.gnu.org/licenses/>.
package org.xalgorithms.rules

import org.bson.BsonDocument
import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.io.Source

import org.xalgorithms.rules.steps._
import org.xalgorithms.rules.elements._

// TODO: This module stills uses null. Convert to Option.

object StepProduce {
  implicit val columnReads: Reads[Column] = (
    (JsPath \ "table").read[String] and
    (JsPath \ "sources").read[JsArray]
  )(produce_column _)

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
    (JsPath \ "table").read[String]
  )(produce_add_revision_source _)
  
  implicit val updateRevisionSourceReads : Reads[UpdateRevisionSource] = (
    (JsPath \ "column").read[String] and
    (JsPath \ "table").read[String]
  )(produce_update_revision_source _)

  implicit val refinementReads: Reads[Refinement] = (
    (JsPath \ "name").read[String] and
    (JsPath \ "condition").readNullable[JsObject] and
    (JsPath \ "assignment").readNullable[JsObject] and
    (JsPath \ "function").readNullable[JsObject]
  )(produce_refinement _)

  implicit val arrangementReads: Reads[Arrangement] = (
    (JsPath \ "type").read[String] and
    (JsPath \ "name").read[String] and
    (JsPath \ "args").read[Seq[Value]]
  )(produce_arrangement _)

  // TODO: eliminate *OrNull in favour of Option[*]
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

  def produce_value(vt: String, content: JsObject): Value = vt match {
    case "string"    => new StringValue(stringOrNull(content, "value"))
    case "number"    => new NumberValue(doubleOrNull(content, "value"))
    case "reference" => new DocumentReferenceValue(
      stringOrNull(content, "section"),
      stringOrNull(content, "key")
    )
    case "function"  => new FunctionValue(
      stringOrNull(content, "name"),
      (content \ "args").validate[Seq[Value]].getOrElse(Seq())
    )
    case _           => null
  }

  def produce_assignment(target: String, source: JsObject): Assignment = {
    return new Assignment(
      target, source.validate[Value].getOrElse(null)
    )
  }

  def produce_revision_source(op: String, source: JsObject): RevisionSource = op match {
    case "add" => source.validate[AddRevisionSource].getOrElse(null)
    case "update" => source.validate[UpdateRevisionSource].getOrElse(null)
    case "delete" => new RemoveRevisionSource(stringOrNull(source, "column"))
    case _ => null
  }

  def produce_add_revision_source(column: String, table: String): AddRevisionSource = {
    return new AddRevisionSource(
      column,
      new TableReference(table)
    )
  }

  def produce_update_revision_source(column: String, table: String): UpdateRevisionSource = {
    return new UpdateRevisionSource(
      column,
      new TableReference(table)
    )
  }

  def produce_remove_revision_source(column: String): RemoveRevisionSource = {
    return new RemoveRevisionSource(column)
  }

  def produce_column(table: String, sources: JsArray): Column = {
    return new Column(
      new TableReference(table),
      sources.validate[Seq[ColumnsTableSource]].getOrElse(Seq()) ++
        sources.validate[Seq[ColumnTableSource]].getOrElse(Seq())
    )
  }

  def produce_assemble(content: JsObject): Step = {
    return new AssembleStep(
      stringOrNull(content, "table_name"),
      (content \ "columns").validate[Seq[Column]].getOrElse(null)
    )
  }

  def produce_require(content: JsObject): Step = {
    return new RequireStep(
      produce_packaged_table_reference((content \ "reference").as[JsObject]),
      (content \ "indexes").as[Seq[String]])
  }

  def produce_revise(content: JsObject): Step = {
    return new ReviseStep(
      new TableReference(stringOrNull(content, "table")),
      (content \ "revisions").validate[Seq[RevisionSource]].getOrElse(Seq())
    )
  }

  def produce_take_refinement(
    condition_opt: Option[JsObject],
    function_opt: Option[JsObject]
  ): TakeRefinement = {
    condition_opt match {
      case Some(cond) => new ConditionalTakeRefinement(
        Option(cond.validate[When].getOrElse(null))
      )
      case None => function_opt match {
        case Some(func_content) => {
          val fv = new TakeFunction(
            stringOrNull(func_content, "name"),
            (func_content \ "args").validate[Seq[Value]].getOrElse(Seq())
          )
          new FunctionalTakeRefinement(Some(fv))
        }
        case None => null
      }
    }
  }

  def produce_refinement(
    name: String,
    condition_opt: Option[JsObject],
    assignment_opt: Option[JsObject],
    function_opt: Option[JsObject]
  ): Refinement = name match {
    case "filter" => new FilterRefinement(
      condition_opt.map { cond => cond.validate[When].getOrElse(null) }
    )
    case "map"    => new MapRefinement(
      assignment_opt.map { ass => ass.validate[Assignment].getOrElse(null) }
    )
    case "take"   => produce_take_refinement(condition_opt, function_opt)
    case _        => null
  }

  def produce_arrangement(
    t: String,
    name: String,
    args: Seq[Value]
  ): Arrangement = t match {
    case "function" => new Arrangement(new ArrangeFunction(name, args))
    case _ => null
  }

  def produce_refine(content: JsObject): Step = {
    return new RefineStep(
      new TableReference(stringOrNull(content, "table")),
      (content \ "refined_name").validate[String].getOrElse(null),
      (content \ "refinements").validate[Seq[Refinement]].getOrElse(Seq())
    )
  }

  def produce_arrange(content: JsObject): Step = {
    return new ArrangeStep(
      new TableReference(stringOrNull(content, "table")),
      (content \ "table_name").validate[String].getOrElse(null),
      (content \ "arrangements").validate[Seq[Arrangement]].getOrElse(Seq())
    )
  }

  val fns = Map[String, (JsObject) => Step](
    "assemble" -> produce_assemble,
    "require"  -> produce_require,
    "revise"   -> produce_revise,
    "refine"   -> produce_refine,
    "arrange"  -> produce_arrange,
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
