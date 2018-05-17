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

class FilterStepSpec extends FlatSpec with Matchers with MockFactory {
  "FilterStep" should "transform tables in-place based on " in {
    val ctx = mock[Context]
    val table0 = Seq(
      Map("a" -> new StringValue("00"), "b" -> new StringValue("01")),
      Map("a" -> new StringValue("00"), "b" -> new StringValue("11")),
      Map("a" -> new StringValue("10"), "b" -> new StringValue("11")))
    val section = "table"
    val table_name = "table0"

    val expected = Map("a" -> "00", "b" -> "11")

    val whens = expected.map { case (k, v) =>
      new When(
        new DocumentReferenceValue("_context", k),
        new StringValue(v),
        "eq"
      )
    }.toSeq

    val step = new FilterStep(
      new TableReference(section, table_name),
      whens
    )

    (ctx.lookup_table _).expects(section, table_name).returning(table0)
    (ctx.retain_table _).expects(section, table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(1)
      tbl(0).size shouldEqual(expected.size)
      tbl(0).foreach { case (k, v) =>
        v shouldBe a [StringValue]
        v.asInstanceOf[StringValue].value shouldEqual(expected(k))
      }
    }
    step.execute(ctx)
  }
}
