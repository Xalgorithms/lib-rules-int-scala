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

import org.xalgorithms.rules._
import org.xalgorithms.rules.steps._
import org.xalgorithms.rules.elements._

import scala.io.Source
import org.bson.BsonDocument
import org.scalatest._

class SyntaxSpec extends FlatSpec with Matchers {
  def syntax_from_source(step_name: String): Seq[Step] = {
    SyntaxFromSource(Source.fromURL(getClass.getResource(s"/${step_name}.json")))
  }

  def syntax_from_bson(step_name: String): Seq[Step] = {
    SyntaxFromBson(BsonDocument.parse(Source.fromURL(getClass.getResource(s"/${step_name}.json")).mkString))
  }

  def validate_assemble(steps: Seq[Step]) {
    steps.length shouldBe 1
    steps.head should not be null
    steps.head shouldBe a [AssembleStep]

    val o = steps.head.asInstanceOf[AssembleStep]
    o.name shouldEqual("table_final")

    o.columns.length shouldBe 2

    o.columns(0).table should not be null
    o.columns(0).table.name shouldEqual("table0")
    o.columns(0).sources.length shouldBe 1
    o.columns(0).sources(0) should not be null
    o.columns(0).sources(0) shouldBe a [ColumnsTableSource]
    o.columns(0).sources(0).asInstanceOf[ColumnsTableSource].columns shouldEqual(Seq("c0", "c1", "c2"))
    o.columns(0).sources(0).whens.length shouldBe 1
    o.columns(0).sources(0).whens(0) should not be null
    o.columns(0).sources(0).whens(0).left should not be null
    o.columns(0).sources(0).whens(0).left shouldBe a [ReferenceValue]
    o.columns(0).sources(0).whens(0).left.asInstanceOf[ReferenceValue].section shouldEqual("_context")
    o.columns(0).sources(0).whens(0).left.asInstanceOf[ReferenceValue].key shouldEqual("a")
    o.columns(0).sources(0).whens(0).right should not be null
    o.columns(0).sources(0).whens(0).right shouldBe a [StringValue]
    o.columns(0).sources(0).whens(0).right.asInstanceOf[StringValue].value shouldEqual("a distant ship")
    o.columns(0).sources(0).whens(0).op shouldEqual("eq")

    o.columns(1).table should not be null
    o.columns(1).table.name shouldEqual("table1")
    o.columns(1).sources.length shouldBe 1
    o.columns(1).sources(0) should not be null
    o.columns(1).sources(0) shouldBe a [ColumnTableSource]
    o.columns(1).sources(0).asInstanceOf[ColumnTableSource].name shouldEqual("y")
    o.columns(1).sources(0).asInstanceOf[ColumnTableSource].source shouldEqual("x")
    o.columns(1).sources(0).whens.length shouldBe 1
    o.columns(1).sources(0).whens(0) should not be null
    o.columns(1).sources(0).whens(0).left should not be null
    o.columns(1).sources(0).whens(0).left shouldBe a [ReferenceValue]
    o.columns(1).sources(0).whens(0).left.asInstanceOf[ReferenceValue].section shouldEqual("_local")
    o.columns(1).sources(0).whens(0).left.asInstanceOf[ReferenceValue].key shouldEqual("x")
    o.columns(1).sources(0).whens(0).right should not be null
    o.columns(1).sources(0).whens(0).right shouldBe a [NumberValue]
    o.columns(1).sources(0).whens(0).right.asInstanceOf[NumberValue].value shouldEqual(1.0)
    o.columns(1).sources(0).whens(0).op shouldEqual("eq")
  }

  def validate_require(steps: Seq[Step]) {
    steps.length shouldBe 1
    steps.head should not be null
    steps.head shouldBe a [RequireStep]

    val o = steps.head.asInstanceOf[RequireStep]
    o.table_reference should not be null
    o.table_reference.package_name shouldEqual "package"
    o.table_reference.id shouldEqual "id"
    o.table_reference.version shouldEqual "1.2.34"
    o.table_reference.name shouldEqual "table_name"
    o.indexes shouldEqual Seq("a", "b")
  }

  def validate_revise(steps: Seq[Step]) {
    steps.length shouldBe 1
    steps.head should not be null
    steps.head shouldBe a [ReviseStep]

    val o = steps.head.asInstanceOf[ReviseStep]

    o.table should not be null
    o.table.name shouldEqual("items")

    o.sources.length shouldEqual(3)
    o.sources(0) should not be null
    o.sources(0) shouldBe a [AddRevisionSource]
    o.sources(0).column shouldEqual("a.b")
    o.sources(0).asInstanceOf[TableRevisionSource].table.name shouldEqual("foo")

    o.sources(1) should not be null
    o.sources(1) shouldBe a [UpdateRevisionSource]
    o.sources(1).column shouldEqual("c")
    o.sources(1).asInstanceOf[TableRevisionSource].table.name shouldEqual("bar")

    o.sources(2) should not be null
    o.sources(2) shouldBe a [RemoveRevisionSource]
    o.sources(2).column shouldEqual("d")
  }

  def validate_reference_value(v: Value, ex_section: String, ex_key: String) = {
    v should not be null
    v shouldBe a [ReferenceValue]
    v.asInstanceOf[ReferenceValue].section shouldEqual(ex_section)
    v.asInstanceOf[ReferenceValue].key shouldEqual(ex_key)
  }

  def validate_number_value(v: Value, ex_v: BigDecimal) = {
    v should not be null
    v shouldBe a [NumberValue]
    v.asInstanceOf[NumberValue].value shouldEqual(ex_v)
  }

  def validate_refine(steps: Seq[Step]) {
    steps.length shouldBe 1
    steps.head should not be null
    steps.head shouldBe a [RefineStep]

    val o = steps.head.asInstanceOf[RefineStep]

    o.table should not be null
    o.table.name shouldEqual("x")
    o.refined_name shouldEqual("y")

    o.refinements.length shouldEqual(7)

    o.refinements(0) shouldBe a [FilterRefinement]
    val when0 = o.refinements(0).asInstanceOf[FilterRefinement].when.getOrElse(null)
    when0 should not be null
    validate_reference_value(when0.left, "_column", "a")
    validate_number_value(when0.right, 3.0)
    when0.op shouldEqual("lt")

    o.refinements(1) shouldBe a [MapRefinement]
    val ass0 = o.refinements(1).asInstanceOf[MapRefinement].assignment.getOrElse(null)
    ass0 should not be null
    validate_reference_value(ass0.target, "_column", "a")
    validate_reference_value(ass0.source, "_column", "b")

    o.refinements(2) shouldBe a [MapRefinement]
    val ass1 = o.refinements(2).asInstanceOf[MapRefinement].assignment.getOrElse(null)
    ass1 should not be null
    validate_reference_value(ass1.target, "_local", "a")
    validate_reference_value(ass1.source, "_column", "b")

    o.refinements(3) shouldBe a [MapRefinement]
    val ass2 = o.refinements(3).asInstanceOf[MapRefinement].assignment.getOrElse(null)
    ass2 should not be null
    validate_reference_value(ass2.target, "_local", "a")
    validate_reference_value(ass2.source, "_local", "b")

    o.refinements(4) shouldBe a [MapRefinement]
    val ass3 = o.refinements(4).asInstanceOf[MapRefinement].assignment.getOrElse(null)
    ass3 should not be null
    validate_reference_value(ass3.target, "_local", "a")
    ass3.source should not be null
    ass3.source shouldBe a [FunctionValue]
    ass3.source.asInstanceOf[FunctionValue].name shouldEqual("concat")
    ass3.source.asInstanceOf[FunctionValue].args.length shouldEqual(2)
    validate_reference_value(
      ass3.source.asInstanceOf[FunctionValue].args(0), "_column", "b")
    validate_reference_value(
      ass3.source.asInstanceOf[FunctionValue].args(1), "_column", "c")

    o.refinements(5) shouldBe a [FunctionalTakeRefinement]
    val fn0 = o.refinements(5).asInstanceOf[FunctionalTakeRefinement].func.getOrElse(null)
    fn0 should not be null
    fn0 shouldBe a [TakeFunction]
    fn0.name shouldEqual("nth")
    fn0.args.length shouldEqual(2)
    fn0.args(0) shouldBe a [NumberValue]
    fn0.args(0).asInstanceOf[NumberValue].value shouldEqual(BigDecimal(1.0))
    fn0.args(1) shouldBe a [NumberValue]
    fn0.args(1).asInstanceOf[NumberValue].value shouldEqual(BigDecimal(3.0))

    o.refinements(6) shouldBe a [ConditionalTakeRefinement]
    val when1 = o.refinements(6).asInstanceOf[ConditionalTakeRefinement].when.getOrElse(null)
    when1 should not be null
    validate_reference_value(when1.left, "_column", "a")
    validate_reference_value(when1.right, "_column", "b")
    when1.op shouldEqual("eq")
  }

  def validate_arrange(steps: Seq[Step]) {
    steps.length shouldBe 3

    steps(0) should not be null
    steps(0) shouldBe a [ArrangeStep]
    steps(0).asInstanceOf[ArrangeStep].table.name shouldEqual("x")
    steps(0).asInstanceOf[ArrangeStep].table_name shouldEqual("y")

    steps(0).asInstanceOf[ArrangeStep].arrangements.length shouldEqual(1)

    val arr00 = steps(0).asInstanceOf[ArrangeStep].arrangements(0)
    arr00.func.name shouldEqual("invert")
    arr00.func.args.length shouldEqual(0)

    steps(1) should not be null
    steps(1) shouldBe a [ArrangeStep]
    steps(1).asInstanceOf[ArrangeStep].table.name shouldEqual("a")
    steps(1).asInstanceOf[ArrangeStep].table_name shouldEqual("b")

    steps(1).asInstanceOf[ArrangeStep].arrangements.length shouldEqual(2)

    val arr10 = steps(1).asInstanceOf[ArrangeStep].arrangements(0)
    val arr11 = steps(1).asInstanceOf[ArrangeStep].arrangements(1)

    arr10.func.name shouldEqual("shift")
    arr10.func.args.length shouldEqual(1)
    arr10.func.args(0) shouldBe a [NumberValue]
    arr10.func.args(0).asInstanceOf[NumberValue].value shouldEqual(2.0)

    arr11.func.name shouldEqual("shift")
    arr11.func.args.length shouldEqual(1)
    arr11.func.args(0) shouldBe a [NumberValue]
    arr11.func.args(0).asInstanceOf[NumberValue].value shouldEqual(-3.0)

    steps(2) should not be null
    steps(2) shouldBe a [ArrangeStep]
    steps(2).asInstanceOf[ArrangeStep].table.name shouldEqual("p")
    steps(2).asInstanceOf[ArrangeStep].table_name shouldEqual("q")

    steps(2).asInstanceOf[ArrangeStep].arrangements.length shouldEqual(1)

    val arr20 = steps(2).asInstanceOf[ArrangeStep].arrangements(0)
    arr20.func.name shouldEqual("sort")
    arr20.func.args.length shouldEqual(3)
    arr20.func.args(0) shouldBe a [ReferenceValue]
    arr20.func.args(0).asInstanceOf[ReferenceValue].key shouldEqual("a")
    arr20.func.args(1) shouldBe a [StringValue]
    arr20.func.args(1).asInstanceOf[StringValue].value shouldEqual("alpha")
    arr20.func.args(2) shouldBe a [StringValue]
    arr20.func.args(2).asInstanceOf[StringValue].value shouldEqual("descending")
  }

  "AssembleStep" should "load from JSON" in {
    validate_assemble(syntax_from_source("assemble"))
  }

  it should "load from BsonDocument" in {
    validate_assemble(syntax_from_bson("assemble"))
  }

  it should "optionally load whens" in {
    val steps = syntax_from_source("assemble_no_whens")

    steps.length shouldBe 1
    steps.head should not be null
    steps.head shouldBe a [AssembleStep]

    val o = steps.head.asInstanceOf[AssembleStep]
    o.name shouldEqual("table_final")

    o.columns.length shouldBe 2

    o.columns(0).table should not be null
    o.columns(0).table.name shouldEqual("table0")
    o.columns(0).sources.length shouldBe 1
    o.columns(0).sources(0) should not be null
    o.columns(0).sources(0) shouldBe a [ColumnsTableSource]
    o.columns(0).sources(0).asInstanceOf[ColumnsTableSource].columns shouldEqual(Seq("c0", "c1", "c2"))
    o.columns(0).sources(0).whens.length shouldBe 0

    o.columns(1).table should not be null
    o.columns(1).table.name shouldEqual("table1")
    o.columns(1).sources.length shouldBe 1
    o.columns(1).sources(0) should not be null
    o.columns(1).sources(0) shouldBe a [ColumnTableSource]
    o.columns(1).sources(0).asInstanceOf[ColumnTableSource].name shouldEqual("y")
    o.columns(1).sources(0).asInstanceOf[ColumnTableSource].source shouldEqual("x")
    o.columns(1).sources(0).whens.length shouldBe 0
  }

  it should "load sources with empty columns" in {
    val steps = syntax_from_source("assemble_empty_columns")

    steps.length shouldBe 1
    steps.head should not be null
    steps.head shouldBe a [AssembleStep]

    val o = steps.head.asInstanceOf[AssembleStep]
    o.name shouldEqual("table_final")

    o.columns.length shouldBe 1

    o.columns(0).table should not be null
    o.columns(0).table.name shouldEqual("table0")
    o.columns(0).sources.length shouldBe 1
    o.columns(0).sources(0) should not be null
    o.columns(0).sources(0) shouldBe a [ColumnsTableSource]
    o.columns(0).sources(0).asInstanceOf[ColumnsTableSource].columns shouldEqual(Seq())
    o.columns(0).sources(0).whens.length shouldBe 0
  }

  "RequireStep" should "load from JSON" in {
    validate_require(syntax_from_source("require"))
  }

  it should "load from BsonDocument" in {
    validate_require(syntax_from_bson("require"))
  }

  "ReviseStep" should "load from JSON" in {
    validate_revise(syntax_from_source("revise"))
  }

  it should "load from BsonDocument" in {
    validate_revise(syntax_from_bson("revise"))
  }

  "RefineStep" should "load from JSON" in {
    validate_refine(syntax_from_source("refine"))
  }

  it should "load from BsonDocument" in {
    validate_refine(syntax_from_bson("refine"))
  }

  "ArrangeStep" should "load from JSON" in {
    validate_arrange(syntax_from_source("arrange"))
  }

  it should "load from BsonDocument" in {
    validate_arrange(syntax_from_bson("arrange"))
  }
}
