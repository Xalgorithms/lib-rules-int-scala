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

class RefineStepSpec extends FlatSpec with Matchers with MockFactory {
  val _table = Seq(
    Map("a" -> "1", "b" -> "1"),
    Map("a" -> "2", "b" -> "4"),
    Map("a" -> "3", "b" -> "9"),
    Map("a" -> "4", "b" -> "16"),
    Map("a" -> "5", "b" -> "25")
  ).map { r => r.mapValues { v => new StringValue(v) } }

  "RefineStep" should "first filter then process tables and finally take" in {
    val ctx = mock[Context]
    val secs = mock[Sections]
    val tables = mock[TableSection]

    (ctx.sections _).expects.returning(secs)
    (secs.tables _).expects.returning(tables)

    val section = "table"
    val table_name = "table0"
    val refined_table_name = "table_refined"

    val fr0 = mock[FilterRefinement]
    val fr1 = mock[FilterRefinement]
    val mr = mock[MapRefinement]
    val tr = mock[TakeRefinement]

    val filtered_table = Seq(
      _table(1),
      _table(2),
      _table(4)
    )
    val mapped_table = Seq(
      _table(1) + ("c" -> new StringValue("44")),
      _table(2) + ("c" -> new StringValue("99")),
      _table(4) + ("c" -> new StringValue("2525"))
    )
    val final_table = Seq(
      mapped_table(0),
      mapped_table(2)
    )

    val verify_row = (row: Map[String, IntrinsicValue], ex: Option[Map[String, IntrinsicValue]]) => {
      (ctx: Context, ac_row: Map[String, IntrinsicValue]) => {
        ctx shouldBe a [RowContext]
        ctx.asInstanceOf[RowContext].local_row shouldEqual(row)
        ex
      }
    }
    val verify_no_row = (row: Map[String, IntrinsicValue]) => verify_row(row, None)
    val verify_a_row = (row: Map[String, IntrinsicValue]) => verify_row(row, Some(row))
    val verify_a_new_row = (
      local_row: Map[String, IntrinsicValue],
      new_row: Map[String, IntrinsicValue]
    ) => verify_row(local_row, Some(new_row))

    (fr0.refine _).expects(*, _table(0)) onCall verify_no_row(_table(0))
    (fr0.refine _).expects(*, _table(1)) onCall verify_a_row(_table(1))
    (fr0.refine _).expects(*, _table(2)) onCall verify_a_row(_table(2))
    (fr0.refine _).expects(*, _table(3)) onCall verify_a_row(_table(3))
    (fr0.refine _).expects(*, _table(4)) onCall verify_a_row(_table(4))

    (fr1.refine _).expects(*, _table(1)) onCall verify_a_row(_table(1))
    (fr1.refine _).expects(*, _table(2)) onCall verify_a_row(_table(2))
    (fr1.refine _).expects(*, _table(3)) onCall verify_no_row(_table(3))
    (fr1.refine _).expects(*, _table(4)) onCall verify_a_row(_table(4))

    (mr.refine _).expects(*, filtered_table(0)) onCall verify_a_new_row(filtered_table(0), mapped_table(0))
    (mr.refine _).expects(*, filtered_table(1)) onCall verify_a_new_row(filtered_table(1), mapped_table(1))
    (mr.refine _).expects(*, filtered_table(2)) onCall verify_a_new_row(filtered_table(2), mapped_table(2))

    (tr.refine _).expects(*, mapped_table(0)) onCall verify_a_new_row(mapped_table(0), final_table(0))
    (tr.refine _).expects(*, mapped_table(1)) onCall verify_no_row(mapped_table(1))
    (tr.refine _).expects(*, mapped_table(2)) onCall verify_a_new_row(mapped_table(2), final_table(1))

    (ctx.lookup_table _).expects(section, table_name).returning(_table)
    (tables.retain _).expects(refined_table_name, *) onCall { (name, tbl) =>
      tbl.size shouldEqual(final_table.size)
      (tbl, final_table).zipped.foreach { case (rac, rex) =>
        rex.foreach { case (k, v) =>
          rac(k) shouldBe a [StringValue]
          rac(k).asInstanceOf[StringValue].value shouldEqual(v.asInstanceOf[StringValue].value)
        }
      }
    }

    val step = new RefineStep(
      new TableReference(section, table_name),
      refined_table_name,
      Seq(fr0, mr, fr1, tr))

    step.execute(ctx)
  }
}
