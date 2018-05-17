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
