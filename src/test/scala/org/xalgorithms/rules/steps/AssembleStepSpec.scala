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

import org.scalamock.scalatest.MockFactory
import org.scalatest._

import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._
import org.xalgorithms.rules.steps._

class AssembleStepSpec extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {
  var _ctx: Context = null
  var _secs: Sections = null
  var _tables: TableSection = null

  before {
    _ctx = mock[Context]
    _secs = new Sections
    _tables = _secs.tables()
  }

  def verify_table_named_columns(
    name: String,
    ex: Seq[Map[String, IntrinsicValue]],
    keys: Map[String, String] = Map(),
    skipped_keys: Seq[String] = Seq()
  ) = _tables.lookup(name) match {
    case Some(tbl) => {
      tbl.size shouldEqual(ex.size)
      (tbl, ex).zipped.foreach { case (rac, rex) =>
        val key_checks = if (keys.isEmpty) {
          rex.keys.foldLeft(Map[String, String]()) { case (m, k) => m + (k -> k) }
        } else {
          keys
        }

        key_checks.foreach { case (sk, tk) =>
          rac(tk) shouldBe a [StringValue]
          rac(tk).asInstanceOf[StringValue].value shouldEqual(rex(sk).asInstanceOf[StringValue].value)
        }
        skipped_keys.foreach { k => rac.contains(k) shouldEqual(false) }
      }
    }

    case None => true shouldBe(false)
  }

  def verify_table(
    name: String,
    ex: Seq[Map[String, IntrinsicValue]],
    keys: Seq[String] = Seq(),
    skipped_keys: Seq[String] = Seq()
  ) = {
    verify_table_named_columns(
      name,
      ex,
      keys.foldLeft(Map[String, String]()) { case (m, k) => m + (k -> k) },
      skipped_keys)
  }

  def verify_both_tables(
    name: String,
    tables: Tuple2[Seq[Map[String, IntrinsicValue]], Seq[Map[String, IntrinsicValue]]],
    keys: Tuple2[Seq[String], Seq[String]] = (Seq(), Seq())
  ) = _tables.lookup(name) match {
    case Some (tbl) => {
      tbl.size shouldEqual(tables._1.size * tables._2.size)
      tables._1.indices.foreach { i0 =>
        tables._2.indices.foreach { i1 =>
          val rac = tbl(i0 * tables._2.size + i1)
          keys._1.foreach { k =>
            val rex = tables._1(i0)
            rac(k) shouldBe a [StringValue]
            rac(k).asInstanceOf[StringValue].value shouldEqual(rex(k).asInstanceOf[StringValue].value)
          }
          keys._2.foreach { k =>
            val rex = tables._2(i1)
            rac(k) shouldBe a [StringValue]
            rac(k).asInstanceOf[StringValue].value shouldEqual(rex(k).asInstanceOf[StringValue].value)
          }
        }
      }
    }

    case None => true shouldEqual(false)
  }

  "AssembleStep" should "load all keys using COLUMNS" in {
    val table_name = "table0"
    val final_table_name = "table_final"
    val table = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11")))

    val cols = new Column(
      new TableReference(table_name),
      Seq(new ColumnsTableSource(Seq(), Seq())))

    val step = new AssembleStep(final_table_name, Seq(cols))

    (_ctx.sections _).expects().anyNumberOfTimes.returning(_secs)
    _tables.retain(table_name, table)

    step.execute(_ctx)
    verify_table(final_table_name, table)
  }

  it should "load specific keys using COLUMNS" in {
    val table_name = "table0"
    val final_table_name = "table_final"
    val table = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11")))
    val keys = Seq("a")
    val skipped_keys = Seq("b")

    val cols = new Column(
      new TableReference(table_name),
      Seq(new ColumnsTableSource(keys, Seq())))

    val step = new AssembleStep(final_table_name, Seq(cols))

    (_ctx.sections _).expects().anyNumberOfTimes.returning(_secs)
    _tables.retain(table_name, table)

    step.execute(_ctx)
    verify_table(final_table_name, table, keys, skipped_keys)
  }

  it should "be filtered by WHEN using COLUMNS" in {
    val table_name = "table0"
    val final_table_name = "table_final"
    val table = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("00"), "b" -> new StringValue("11")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11")))
    val table_expected = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("11"))
    )

    val whens = Seq(
      new When(new StringValue("11"), new DocumentReferenceValue("_context", "b"), "eq"),
      new When(new StringValue("00"), new DocumentReferenceValue("_context", "a"), "eq"))
    val cols = new Column(
      new TableReference(table_name),
      Seq(new ColumnsTableSource(Seq(), whens)))

    val step = new AssembleStep(final_table_name, Seq(cols))

    (_ctx.sections _).expects().anyNumberOfTimes.returning(_secs)
    _tables.retain(table_name, table)

    step.execute(_ctx)
    verify_table(final_table_name, table_expected)
  }

  it should "perform a cross-product of multiple COLUMNS from different tables" in {
    val table0_name = "table0"
    val table1_name = "table1"
    val final_table_name = "table_final"
    val table0 = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11"))
    )
    val table1 = Seq(
      Map("c" -> new StringValue("02"), "d" -> new StringValue("03")),
      Map("c" -> new StringValue("12"), "d" -> new StringValue("13"))
    )
    val keys0 = Seq("a")
    val keys1 = Seq("c", "d")

    val cols0 = new Column(
      new TableReference(table0_name),
      Seq(
        new ColumnsTableSource(keys0, Seq())
      )
    )
    val cols1 = new Column(
      new TableReference(table1_name),
      Seq(
        new ColumnsTableSource(keys1, Seq())
      )
    )

    val step = new AssembleStep(final_table_name, Seq(cols0, cols1))

    (_ctx.sections _).expects().anyNumberOfTimes.returning(_secs)

    _tables.retain(table0_name, table0)
    _tables.retain(table1_name, table1)

    step.execute(_ctx)
    verify_both_tables(final_table_name, (table0, table1), (keys0, keys1))
  }

  it should "merge multiple COLUMNS from the same table" in {
    val table_name = "table0"
    val final_table_name = "table_final"
    val table = Seq(
      Map(
        "a" -> new StringValue("00"), "b" -> new StringValue("01"),
        "c" -> new StringValue("02"), "d" -> new StringValue("03")),
      Map(
        "a" -> new StringValue("10"), "b" -> new StringValue("11"),
        "c" -> new StringValue("12"), "d" -> new StringValue("13"))
    )
    val keys0 = Seq("a")
    val keys1 = Seq("c", "d")
    val skipped_keys = Seq("b")

    val cols = new Column(
      new TableReference(table_name),
      Seq(
        new ColumnsTableSource(keys0, Seq()),
        new ColumnsTableSource(keys1, Seq())
      )
    )

    val step = new AssembleStep(final_table_name, Seq(cols))

    (_ctx.sections _).expects().anyNumberOfTimes.returning(_secs)
    _tables.retain(table_name, table)

    step.execute(_ctx)
    verify_table(final_table_name, table, keys0 ++ keys1, skipped_keys)
  }

  it should "load a single column using COLUMN" in {
    val table_name = "table0"
    val final_table_name = "table_final"
    val table = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11")))
    val sk = "b"
    val tk = "bbb"

    val cols = new Column(
      new TableReference(table_name),
      Seq(new ColumnTableSource(tk, sk, Seq())))

    val step = new AssembleStep(final_table_name, Seq(cols))

    (_ctx.sections _).expects().anyNumberOfTimes.returning(_secs)
    _tables.retain(table_name, table)

    step.execute(_ctx)
    verify_table_named_columns(final_table_name, table, Map(sk -> tk))
  }

  it should "perform a cross-product of multiple COLUMN from different tables" in {
    val ctx = mock[Context]
    val table0_name = "table0"
    val table1_name = "table1"
    val final_table_name = "table_final"
    val table0 = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11"))
    )
    val table1 = Seq(
      Map("c" -> new StringValue("02"), "d" -> new StringValue("03")),
      Map("c" -> new StringValue("12"), "d" -> new StringValue("13"))
    )
    val table0_key = "a"
    val table1_key = "d"

    val cols0 = new Column(
      new TableReference(table0_name),
      Seq(
        new ColumnTableSource(table0_key, table0_key, Seq())
      )
    )
    val cols1 = new Column(
      new TableReference(table1_name),
      Seq(
        new ColumnTableSource(table1_key, table1_key, Seq())
      )
    )

    val step = new AssembleStep(final_table_name, Seq(cols0, cols1))

    (_ctx.sections _).expects().anyNumberOfTimes.returning(_secs)
    _tables.retain(table0_name, table0)
    _tables.retain(table1_name, table1)

    step.execute(_ctx)
    verify_both_tables(
      final_table_name, (table0, table1), (Seq(table0_key), Seq(table1_key)))
  }

  it should "be filtered by WHEN using COLUMN" in {
    val table_name = "table0"
    val final_table_name = "table_final"
    val table = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("00"), "b" -> new StringValue("11")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11")))
    val table_expected = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("11"))
    )
    val sk = "b"
    val tk = "bbb"

    val whens = Seq(
      new When(new StringValue("11"), new DocumentReferenceValue("_context", "b"), "eq"),
      new When(new StringValue("00"), new DocumentReferenceValue("_context", "a"), "eq"))
    val cols = new Column(
      new TableReference(table_name),
      Seq(new ColumnTableSource(tk, sk, whens)))

    val step = new AssembleStep(final_table_name, Seq(cols))

    (_ctx.sections _).expects().anyNumberOfTimes.returning(_secs)

    _tables.retain(table_name, table)

    step.execute(_ctx)
    verify_table_named_columns(final_table_name, table_expected, Map(sk -> tk))
  }

  it should "combine COLUMNS with additional COLUMN using a merge" in {
    val final_table_name = "table_final"
    val table0_name = "table0"
    val table0 = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11")),
      Map("a" -> new StringValue("20"), "b" -> new StringValue("21"))
    )
    val table1_name = "table1"
    val t1k0 = "x"
    val t1k1 = "y"
    val table1 = Seq(
      Map("x" -> new StringValue("x0"), "y" -> new StringValue("y0")),
      Map("x" -> new StringValue("x1"), "y" -> new StringValue("y1"))
    )
      
    val table0_col = new Column(
      new TableReference(table0_name),
      Seq(new ColumnsTableSource(Seq(), Seq()))
    )
    val table1_col = new Column(
      new TableReference(table1_name),
      Seq(
        new ColumnTableSource(t1k0, t1k0, Seq()),
        new ColumnTableSource(t1k1, t1k1, Seq())
      )
    )

    val step = new AssembleStep(final_table_name, Seq(table0_col, table1_col))

    (_ctx.sections _).expects().anyNumberOfTimes.returning(_secs)
    _tables.retain(table0_name, table0)
    _tables.retain(table1_name, table1)

    step.execute(_ctx)
    _tables.lookup(final_table_name) match {
      case Some(tbl) => {
        tbl.size shouldEqual(table0.size * table1.size)
        table0.indices.foreach { i0 =>
          table1.indices.foreach { i1 =>
            val rac = tbl(2 * i0 + i1)
            Seq(t1k0, t1k1).foreach { k =>
              rac(k) shouldBe a [StringValue]
              rac(k).asInstanceOf[StringValue].value shouldEqual(table1(i1)(k).asInstanceOf[StringValue].value)
            }
            table0(i0).foreach { case (k0, v0) =>
              rac(k0) shouldBe a [StringValue]
              rac(k0).asInstanceOf[StringValue].value shouldEqual(v0.asInstanceOf[StringValue].value)
            }
          }
        }
      }

      case None => true shouldEqual(false)
    }
  }
}
