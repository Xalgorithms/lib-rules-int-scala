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

  val verify_row_context = (ex: Map[String, IntrinsicValue]) => {
    (call_ctx: Context, ac_row: Map[String, IntrinsicValue]) => {
      call_ctx shouldBe a [RowContext]
      call_ctx.asInstanceOf[RowContext].local_row shouldEqual(ex)
    } : Unit
  }

  "RefineStep" should "support filter" in {
    val ctx = mock[Context]
    val section = "table"
    val table_name = "table0"
    val refined_table_name = "table_refined"

    val fr0 = mock[FilterRefinement]
    val fr1 = mock[FilterRefinement]

    val filtered_table0 = Seq(
      _table(1),
      _table(2),
      _table(3),
      _table(4)
    )
    val filtered_table1 = Seq(
      _table(1),
      _table(2),
      _table(4)
    )

    _table.foreach { row =>
      (fr0.add _).expects(*, row) onCall verify_row_context(row)
    }
    (fr0.refine _).expects().returning(filtered_table0)

    filtered_table0.foreach { row =>
      (fr1.add _).expects(*, row) onCall verify_row_context(row)
    }
    (fr1.refine _).expects().returning(filtered_table1)

    (ctx.lookup_table _).expects(section, table_name).returning(_table)
    (ctx.retain_table _).expects(section, refined_table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(filtered_table1.size)
      (tbl, filtered_table1).zipped.foreach { case (rac, rex) =>
        rex.foreach { case (k, v) =>
          rac(k) shouldBe a [StringValue]
          rac(k).asInstanceOf[StringValue].value shouldEqual(v.asInstanceOf[StringValue].value)
        }
      }
    }

    val step = new RefineStep(
      new TableReference(section, table_name),
      refined_table_name,
      Seq(fr0, fr1))

    step.execute(ctx)
  }

  it should "support map" in {
    val ctx = mock[Context]
    val section = "table"
    val table_name = "table0"
    val refined_table_name = "table_refined"

    val mr = mock[MapRefinement]

    val mapped_table = Seq(
      _table(0) + ("c" -> new StringValue("22")),
      _table(1) + ("c" -> new StringValue("44")),
      _table(2) + ("c" -> new StringValue("99")),
      _table(3) + ("c" -> new StringValue("2525")),
      _table(4) + ("c" -> new StringValue("5225"))
    )

    _table.foreach { row =>
      (mr.add _).expects(*, row) onCall verify_row_context(row)
    }
    (mr.refine _).expects().returning(mapped_table)

    (ctx.lookup_table _).expects(section, table_name).returning(_table)
    (ctx.retain_table _).expects(section, refined_table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(mapped_table.size)
      (tbl, mapped_table).zipped.foreach { case (rac, rex) =>
        rex.foreach { case (k, v) =>
          rac(k) shouldBe a [StringValue]
          rac(k).asInstanceOf[StringValue].value shouldEqual(v.asInstanceOf[StringValue].value)
        }
      }
    }

    val step = new RefineStep(
      new TableReference(section, table_name),
      refined_table_name,
      Seq(mr))

    step.execute(ctx)
  }

  it should "support take" in {
    val ctx = mock[Context]
    val section = "table"
    val table_name = "table0"
    val refined_table_name = "table_refined"

    val tr = mock[TakeRefinement]

    val final_table = Seq(
      _table(0),
      _table(2)
    )

    _table.foreach { row =>
      (tr.add _).expects(*, row) onCall verify_row_context(row)
    }
    (tr.refine _).expects().returning(final_table)

    (ctx.lookup_table _).expects(section, table_name).returning(_table)
    (ctx.retain_table _).expects(section, refined_table_name, *) onCall { (section, name, tbl) =>
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
      Seq(tr))

    step.execute(ctx)
  }

  it should "first filter then map and finally take" in {
    val ctx = mock[Context]
    val section = "table"
    val table_name = "table0"
    val refined_table_name = "table_refined"

    val fr0 = mock[FilterRefinement]
    val fr1 = mock[FilterRefinement]
    val mr = mock[MapRefinement]
    val tr = mock[TakeRefinement]

    val filtered_table0 = Seq(
      _table(1),
      _table(2),
      _table(3),
      _table(4)
    )
    val filtered_table1 = Seq(
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

    _table.foreach { row =>
      (fr0.add _).expects(*, row) onCall verify_row_context(row)
    }
    (fr0.refine _).expects().returning(filtered_table0)

    filtered_table0.foreach { row =>
      (fr1.add _).expects(*, row) onCall verify_row_context(row)
    }
    (fr1.refine _).expects().returning(filtered_table1)

    filtered_table1.foreach { row =>
      (mr.add _).expects(*, row) onCall verify_row_context(row)
    }
    (mr.refine _).expects().returning(mapped_table)

    mapped_table.foreach { row =>
      (tr.add _).expects(*, row) onCall verify_row_context(row)
    }
    (tr.refine _).expects().returning(final_table)

    (ctx.lookup_table _).expects(section, table_name).returning(_table)
    (ctx.retain_table _).expects(section, refined_table_name, *) onCall { (section, name, tbl) =>
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
