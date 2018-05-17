// Copyright (C) 2018 Don Kelly <karfai@gmail.com>
// Copyright (C) 2018 Hayk Pilosyan <hayk.pilos@gmail.com>

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
