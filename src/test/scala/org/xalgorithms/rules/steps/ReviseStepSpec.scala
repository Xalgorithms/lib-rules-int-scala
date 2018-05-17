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

import org.scalatest._
import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._
import org.xalgorithms.rules.steps._

class ReviseStepSpec extends FlatSpec with Matchers {
  "ReviseStep" should "produce updates for existing tables" in {
    val ctx = new GlobalContext(new ResourceLoadTableSource())
    ctx.load(new PackagedTableReference("package", "table2", "0.0.1", "table2"))
    ctx.load(new PackagedTableReference("package", "table2_updates", "0.0.1", "table2_updates"))

    val step = new ReviseStep(
      new TableReference("table", "table2"),
      Seq(
        new UpdateRevisionSource(
          "a", Seq(), new TableReference("table", "table2_updates")
        ),
        new UpdateRevisionSource(
          "b", Seq(), new TableReference("table", "table2_updates")
        )
      )
    )

    step.execute(ctx)

    val all_revs = ctx.revisions()
    all_revs should not be null
    all_revs.exists(_._1 == "table2") shouldBe true

    val revs = all_revs("table2")
    revs.length shouldEqual(1)

    val rev0 = revs(0)
    rev0.changes should not be null
    rev0.changes.length shouldEqual(5)

    val a_vals = Seq(10.0, 20.0, 30.0, 30.0, 40.0)
    val b_vals = Seq("FOO", "BAR", "BAZ", "FOO", "FIB")

    rev0.changes.indices.foreach { i =>
      rev0.changes()(i) should not be null

      rev0.changes()(i)("a") should not be null
      rev0.changes()(i)("a").op shouldEqual(ChangeOps.Update)
      rev0.changes()(i)("a").value shouldBe a [NumberValue]
      rev0.changes()(i)("a").value.asInstanceOf[NumberValue].value shouldEqual(a_vals(i))

      rev0.changes()(i)("b") should not be null
      rev0.changes()(i)("b").op shouldEqual(ChangeOps.Update)
      rev0.changes()(i)("b").value shouldBe a [StringValue]
      rev0.changes()(i)("b").value.asInstanceOf[StringValue].value shouldEqual(b_vals(i))

      rev0.changes()(i).exists(_._1 == "c") shouldBe false
      rev0.changes()(i).exists(_._1 == "d") shouldBe false
    }
  }
}
