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
package org.xalgorithms.rules.steps

import org.scalamock.scalatest.MockFactory
import org.scalatest._

import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._
import org.xalgorithms.rules.steps._

class AssembleStepSpec extends FlatSpec with Matchers with MockFactory {
  "AssembleStep" should "load all keys using COLUMNS" in {
    val ctx = mock[Context]
    val section = "table"
    val table_name = "table0"
    val final_table_name = "table_final"
    val table = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11")))

    val cols = new Column(
      new TableReference(section, table_name),
      Seq(new ColumnsTableSource(Seq(), Seq())))

    val step = new AssembleStep(final_table_name, Seq(cols))

    (ctx.lookup_table _).expects(section, table_name).returning(table)
    (ctx.retain_table _) expects(section, final_table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(table.size)
      (tbl, table).zipped.foreach { case (rac, rex) =>
        Seq("a", "b").foreach { k =>
          rac(k) shouldBe a [StringValue]
          rac(k).asInstanceOf[StringValue].value shouldEqual(rex(k).asInstanceOf[StringValue].value)
        }
      }
    }

    step.execute(ctx)
  }

  it should "load specific keys using COLUMNS" in {
    val ctx = mock[Context]
    val section = "table"
    val table_name = "table0"
    val final_table_name = "table_final"
    val table = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11")))
    val keys = Seq("a")
    val skipped_keys = Seq("b")

    val cols = new Column(
      new TableReference(section, table_name),
      Seq(new ColumnsTableSource(keys, Seq())))

    val step = new AssembleStep(final_table_name, Seq(cols))

    (ctx.lookup_table _).expects(section, table_name).returning(table)
    (ctx.retain_table _) expects(section, final_table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(table.size)
      (tbl, table).zipped.foreach { case (rac, rex) =>
        keys.foreach { k =>
          rac(k) shouldBe a [StringValue]
          rac(k).asInstanceOf[StringValue].value shouldEqual(rex(k).asInstanceOf[StringValue].value)
        }
        skipped_keys.foreach { k => rac.contains(k) shouldEqual(false) }
      }
    }

    step.execute(ctx)
  }

  it should "be filtered by WHEN using COLUMNS" in {
    val ctx = mock[Context]
    val section = "table"
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
      new TableReference(section, table_name),
      Seq(new ColumnsTableSource(Seq(), whens)))

    val step = new AssembleStep(final_table_name, Seq(cols))

    (ctx.lookup_table _).expects(section, table_name).returning(table)
    (ctx.retain_table _) expects(section, final_table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(table_expected.size)
      (tbl, table_expected).zipped.foreach { case (rac, rex) =>
        Seq("a", "b").foreach { k =>
          rac(k) shouldBe a [StringValue]
          rac(k).asInstanceOf[StringValue].value shouldEqual(rex(k).asInstanceOf[StringValue].value)
        }
      }
    }

    step.execute(ctx)
  }

  it should "perform a cross-product of multiple COLUMNS from different tables" in {
    val ctx = mock[Context]
    val section = "table"
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
      new TableReference(section, table0_name),
      Seq(
        new ColumnsTableSource(keys0, Seq())
      )
    )
    val cols1 = new Column(
      new TableReference(section, table1_name),
      Seq(
        new ColumnsTableSource(keys1, Seq())
      )
    )

    val step = new AssembleStep(final_table_name, Seq(cols0, cols1))

    (ctx.lookup_table _).expects(section, table0_name).returning(table0)
    (ctx.lookup_table _).expects(section, table1_name).returning(table1)
    (ctx.retain_table _) expects(section, final_table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(table0.size * table1.size)
      table0.indices.foreach { i0 =>
        table1.indices.foreach { i1 =>
          val rac = tbl(i0 * table1.size + i1)
          keys0.foreach { k =>
            val rex = table0(i0)
            rac(k) shouldBe a [StringValue]
            rac(k).asInstanceOf[StringValue].value shouldEqual(rex(k).asInstanceOf[StringValue].value)
          }
          keys1.foreach { k =>
            val rex = table1(i1)
            rac(k) shouldBe a [StringValue]
            rac(k).asInstanceOf[StringValue].value shouldEqual(rex(k).asInstanceOf[StringValue].value)
          }
        }
      }
    }

    step.execute(ctx)
  }

  it should "merge multiple COLUMNS from the same table" in {
    val ctx = mock[Context]
    val section = "table"
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
      new TableReference(section, table_name),
      Seq(
        new ColumnsTableSource(keys0, Seq()),
        new ColumnsTableSource(keys1, Seq())
      )
    )

    val step = new AssembleStep(final_table_name, Seq(cols))

    (ctx.lookup_table _).expects(section, table_name).returning(table)
    (ctx.retain_table _) expects(section, final_table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(table.size)
      (tbl, table).zipped.foreach { case (rac, rex) =>
        (keys0 ++ keys1).foreach { k =>
          rac(k) shouldBe a [StringValue]
          rac(k).asInstanceOf[StringValue].value shouldEqual(rex(k).asInstanceOf[StringValue].value)
        }
        skipped_keys.foreach { k => rac.contains(k) shouldEqual(false) }
      }
    }

    step.execute(ctx)
  }

  it should "perform a cross-product of multiple COLUMN from different tables" in {
    val ctx = mock[Context]
    val section = "table"
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
      new TableReference(section, table0_name),
      Seq(
        new ColumnTableSource(table0_key, table0_key, Seq())
      )
    )
    val cols1 = new Column(
      new TableReference(section, table1_name),
      Seq(
        new ColumnTableSource(table1_key, table1_key, Seq())
      )
    )

    val step = new AssembleStep(final_table_name, Seq(cols0, cols1))

    (ctx.lookup_table _).expects(section, table0_name).returning(table0)
    (ctx.lookup_table _).expects(section, table1_name).returning(table1)
    (ctx.retain_table _) expects(section, final_table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(table0.size * table1.size)
      table0.indices.foreach { i0 =>
        table1.indices.foreach { i1 =>
          val rac = tbl(i0 * table1.size + i1)
          val v0 = table0(i0)(table0_key)
          val v1 = table1(i1)(table1_key)

          rac(table0_key) shouldBe a [StringValue]
          rac(table0_key).asInstanceOf[StringValue].value shouldEqual(v0.asInstanceOf[StringValue].value)

          rac(table1_key) shouldBe a [StringValue]
          rac(table1_key).asInstanceOf[StringValue].value shouldEqual(v1.asInstanceOf[StringValue].value)
        }
      }
    }

    step.execute(ctx)
  }

  it should "merge multiple COLUMN from the same table" in {
    val ctx = mock[Context]
    val section = "table"
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
    val key0 = "a"
    val key1 = "d"
    val skipped_keys = Seq("b", "c")

    val cols = new Column(
      new TableReference(section, table_name),
      Seq(
        new ColumnTableSource(key0, key0, Seq()),
        new ColumnTableSource(key1, key1, Seq())
      )
    )

    val step = new AssembleStep(final_table_name, Seq(cols))

    (ctx.lookup_table _).expects(section, table_name).returning(table)
    (ctx.retain_table _) expects(section, final_table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(table.size)
      (tbl, table).zipped.foreach { case (rac, rex) =>
        Seq(key0, key1).foreach { k =>
          rac(k) shouldBe a [StringValue]
          rac(k).asInstanceOf[StringValue].value shouldEqual(rex(k).asInstanceOf[StringValue].value)
        }
        skipped_keys.foreach { k => rac.contains(k) shouldEqual(false) }
      }
    }

    step.execute(ctx)
  }

  it should "load a single column using COLUMN" in {
    val ctx = mock[Context]
    val section = "table"
    val table_name = "table0"
    val final_table_name = "table_final"
    val table = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11")))
    val sk = "b"
    val tk = "bbb"

    val cols = new Column(
      new TableReference(section, table_name),
      Seq(new ColumnTableSource(tk, sk, Seq())))

    val step = new AssembleStep(final_table_name, Seq(cols))

    (ctx.lookup_table _).expects(section, table_name).returning(table)
    (ctx.retain_table _) expects(section, final_table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(table.size)
      (tbl, table).zipped.foreach { case (rac, rex) =>
        rac(tk) shouldBe a [StringValue]
        rac(tk).asInstanceOf[StringValue].value shouldEqual(rex(sk).asInstanceOf[StringValue].value)
      }
    }

    step.execute(ctx)
  }

  it should "be filtered by WHEN using COLUMN" in {
    val ctx = mock[Context]
    val section = "table"
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
      new TableReference(section, table_name),
      Seq(new ColumnTableSource(tk, sk, whens)))

    val step = new AssembleStep(final_table_name, Seq(cols))

    (ctx.lookup_table _).expects(section, table_name).returning(table)
    (ctx.retain_table _) expects(section, final_table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(table_expected.size)
      (tbl, table_expected).zipped.foreach { case (rac, rex) =>
        rac(tk) shouldBe a [StringValue]
        rac(tk).asInstanceOf[StringValue].value shouldEqual(rex(sk).asInstanceOf[StringValue].value)
      }
    }

    step.execute(ctx)
  }

  it should "combine COLUMNS with additional COLUMN using a merge" in {
    val ctx = mock[Context]
    val section = "table"
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
      new TableReference(section, table0_name),
      Seq(new ColumnsTableSource(Seq(), Seq()))
    )
    val table1_col = new Column(
      new TableReference(section, table1_name),
      Seq(
        new ColumnTableSource(t1k0, t1k0, Seq()),
        new ColumnTableSource(t1k1, t1k1, Seq())
      )
    )

    val step = new AssembleStep(final_table_name, Seq(table0_col, table1_col))

    (ctx.lookup_table _).expects(section, table0_name).returning(table0)
    (ctx.lookup_table _).expects(section, table1_name).returning(table1)

    (ctx.retain_table _) expects(section, final_table_name, *) onCall { (section, name, tbl) =>
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

    step.execute(ctx)
  }
}
