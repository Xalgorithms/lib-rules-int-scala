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
package org.xalgorithms.rules.steps

import com.github.javafaker.Faker
import org.scalamock.scalatest.MockFactory
import org.scalatest._

import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._
import org.xalgorithms.rules.steps._

class ArrangeStepSpec extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {
  var _table: Seq[Map[String, IntrinsicValue]] = null
  var _ctx: Context = null
  var _secs: Sections = null
  var _tables: TableSection = null

  before {
    _table = Seq(
      Map("a" -> new NumberValue(2.0), "b" -> new StringValue("B")),
      Map("a" -> new NumberValue(4.0), "b" -> new StringValue("C")),
      Map("a" -> new NumberValue(1.0), "b" -> new StringValue("A")),
      Map("a" -> new NumberValue(5.0), "b" -> new StringValue("E")),
      Map("a" -> new NumberValue(3.0), "b" -> new StringValue("D"))
    )
    _ctx = mock[Context]
    _secs = new Sections()
    _tables = _secs.tables()
  }

  val _table_name = "table0"

  def verify_table(name: String, ex: Seq[Map[String, IntrinsicValue]]) = _tables.lookup(name) match {
    case Some(tbl) => {
      tbl.size shouldEqual(ex.size)
      (tbl, ex).zipped.foreach { case (rac, rex) =>
        rac("a").asInstanceOf[NumberValue].value shouldEqual(rex("a").asInstanceOf[NumberValue].value)
        rac("b").asInstanceOf[StringValue].value shouldEqual(rex("b").asInstanceOf[StringValue].value)
      }
    }

    case None => true shouldEqual(false)
  }

  def verify_step(
    ex_table_name: String,
    ex_table: Seq[Map[String, IntrinsicValue]],
    func: ArrangeFunction
  ) = {
    _tables.retain(_table_name, _table)

    val step = new ArrangeStep(
      new TableReference(_table_name),
      ex_table_name,
      Seq(new Arrangement(func))
    )

    step.execute(_ctx)
    verify_table(ex_table_name, ex_table)
  }

  def verify_sort_step(
    ex_table_name: String,
    ex_table: Seq[Map[String, IntrinsicValue]],
    col: String = null,
    args: Seq[String] = Seq()
  ) = {
    _tables.retain(_table_name, _table)

    val vargs = if (col != null) {
      Seq(new SectionReferenceValue("_local", col)) ++ args.map(new StringValue(_))
    } else {
      Seq()
    }

    val step = new ArrangeStep(
      new TableReference(_table_name),
      ex_table_name,
      Seq(new Arrangement(new ArrangeFunction("sort", vargs)))
    )

    step.execute(_ctx)
    verify_table(ex_table_name, ex_table)
  }

  "ArrangeStep" should "shift rows in the table" in {
    val ex_table0 = Seq(
      _table(2),
      _table(3),
      _table(4),
      _table(0),
      _table(1),
    )
    val ex_table1 = Seq(
      _table(4),
      _table(0),
      _table(1),
      _table(2),
      _table(3),
    )

    (_ctx.sections _).expects().anyNumberOfTimes.returning(_secs)

    verify_step("table_ex0", ex_table0, new ArrangeFunction("shift", Seq(new NumberValue(-2.0))))
    verify_step("table_ex1", ex_table1, new ArrangeFunction("shift", Seq(new NumberValue(1.0))))
    // // no shift yields original
    verify_step("table_ex2", _table, new ArrangeFunction("shift", Seq(new NumberValue(0.0))))
    // // no args yields original
    verify_step("table_ex3", _table, new ArrangeFunction("shift", Seq()))
    verify_step("table_ex4", _table, new ArrangeFunction("shift", Seq(new StringValue("asdfg"))))
  }

  it should "invert the rows in the table" in {
    val ex_table = Seq(
      _table(4),
      _table(3),
      _table(2),
      _table(1),
      _table(0),
    )

    (_ctx.sections _).expects().anyNumberOfTimes.returning(_secs)

    verify_step("table_ex0", ex_table, new ArrangeFunction("invert", Seq()))
  }

  it should "sort the rows in the table" in {
    // numeric, descending
    val ex_table0 = Seq(
      _table(3),
      _table(1),
      _table(4),
      _table(0),
      _table(2),
    )
    // numeric, ascending
    val ex_table1 = ex_table0.reverse

    // alpha, descending
    val ex_table2 = Seq(
      _table(3),
      _table(4),
      _table(1),
      _table(0),
      _table(2),
    )
    // alpha, ascending
    val ex_table3 = ex_table2.reverse

    (_ctx.sections _).expects().anyNumberOfTimes.returning(_secs)

    verify_sort_step("table_ex0", ex_table0, "a", Seq("numeric", "descending"))
    verify_sort_step("table_ex1", ex_table1, "a", Seq("numeric", "ascending"))
    // default is ascending
    verify_sort_step("table_ex1_default", ex_table1, "a", Seq("numeric"))

    verify_sort_step("table_ex2", ex_table2, "b", Seq("alpha", "descending"))
    verify_sort_step("table_ex3", ex_table3, "b", Seq("alpha", "ascending"))
    // default is ascending
    verify_sort_step("table_ex3_default", ex_table3, "b", Seq("alpha"))

    // default is alpha, ascending
    verify_sort_step("table_ex3_default", ex_table3, "b", Seq())

    // unknown column
    verify_sort_step("table_ex3_default", _table, "XX")

    // no column
    verify_sort_step("table_ex3_default", _table)

    // // unknown sort type
    verify_sort_step("table_ex1_default", _table, "a", Seq("random"))
  }
}
