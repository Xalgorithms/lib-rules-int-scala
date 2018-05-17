// Copyright 2018 Don Kelly <karfai@gmail.com>

// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License. You may
// obtain a copy of the License at

// http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied. See the License for the specific language governing
// permissions and limitations under the License.

package org.xalgorithms.rules.steps

import org.scalamock.scalatest.MockFactory
import org.scalatest._

import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._
import org.xalgorithms.rules.steps._

class MapStepSpec extends FlatSpec with Matchers with MockFactory {
  "MapStep" should "transform tables in place" in {
    val ctx = mock[Context]
    val table0 = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("00"), "b" -> new StringValue("11")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11")))
    val table_expected = table0.map { r =>
      r ++ Map("c" -> r("b"), "d" -> r("a"), "e" -> new StringValue(r("a").value + "_tail"))
    }
    val section = "table"
    val table_name = "table0"

    val step = new MapStep(
      new TableReference(section, table_name),
      Seq(
        new Assignment("c", new DocumentReferenceValue("_context", "b")),
        new Assignment("d", new DocumentReferenceValue("_context", "a")),
        new Assignment("e", new FunctionValue(
          "add", Seq(new DocumentReferenceValue("_context", "a"), new StringValue("_tail"))
        ))
      )
    )

    (ctx.lookup_table _).expects(section, table_name).returning(table0)
    (ctx.retain_table _).expects(section, table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(table_expected.size)
      (tbl, table_expected).zipped.foreach { case (rac, rex) =>
        rex.foreach { case (k, v) =>
          rac(k) shouldBe a [StringValue]
          rac(k).asInstanceOf[StringValue].value shouldEqual(v.asInstanceOf[StringValue].value)
        }
      }
    }
    step.execute(ctx)
  }

  it should "update the _local context as each Assignment is resolved" in {
    val ctx = mock[Context]
    val table0 = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("00"), "b" -> new StringValue("11")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11")))
    val table_expected = table0.map { r =>
      r ++ Map("c" -> r("b"), "d" -> r("b"), "e" -> r("b"))
    }
    val section = "table"
    val table_name = "table0"

    val step = new MapStep(
      new TableReference(section, table_name),
      Seq(
        new Assignment("c", new DocumentReferenceValue("_context", "b")),
        new Assignment("d", new DocumentReferenceValue("_local", "c")),
        new Assignment("e", new DocumentReferenceValue("_local", "d"))
      )
    )

    (ctx.lookup_table _).expects(section, table_name).returning(table0)
    (ctx.retain_table _).expects(section, table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(table_expected.size)
      (tbl, table_expected).zipped.foreach { case (rac, rex) =>
        rex.foreach { case (k, v) =>
          rac(k) shouldBe a [StringValue]
          rac(k).asInstanceOf[StringValue].value shouldEqual(v.asInstanceOf[StringValue].value)
        }
      }
    }
    step.execute(ctx)
  }
}
