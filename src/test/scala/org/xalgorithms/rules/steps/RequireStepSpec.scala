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

import org.scalatest._
import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._
import org.xalgorithms.rules.steps._

class RequireSpec extends FlatSpec with Matchers {
  "RequireStep" should "load tables into the GlobalContext" in {
    val ctx = new GlobalContext(new ResourceLoadTableSource())

    val ref = new PackagedTableReference("package", "table0", "0.0.1", "table0")
    val step = new RequireStep(ref, Seq())

    step.execute(ctx)

    val tbl = ctx.sections.tables.lookup("table0").getOrElse(null)

    tbl should not be null
    tbl.length shouldEqual(3)
    tbl(0)("a") shouldBe a [NumberValue]
    tbl(0)("a").asInstanceOf[NumberValue].value shouldEqual(1.0)
    tbl(0)("b") shouldBe a [StringValue]
    tbl(0)("b").asInstanceOf[StringValue].value shouldEqual("foo")
    tbl(1)("a") shouldBe a [NumberValue]
    tbl(1)("a").asInstanceOf[NumberValue].value shouldEqual(2.0)
    tbl(1)("b") shouldBe a [StringValue]
    tbl(1)("b").asInstanceOf[StringValue].value shouldEqual("bar")
    tbl(2)("a") shouldBe a [NumberValue]
    tbl(2)("a").asInstanceOf[NumberValue].value shouldEqual(3.0)
    tbl(2)("b") shouldBe a [StringValue]
    tbl(2)("b").asInstanceOf[StringValue].value shouldEqual("baz")
  }

  it should "flatten JSON hierarchies when loading" in {
    val ctx = new GlobalContext(new ResourceLoadTableSource())

    val ref = new PackagedTableReference("package", "table0", "0.0.1", "table_hier")
    val step = new RequireStep(ref, Seq())

    step.execute(ctx)

    val tbl = ctx.sections.tables.lookup("table_hier").getOrElse(null)

    tbl should not be null
    tbl.length shouldEqual(2)

    tbl(0).exists(_._1 == "a.aa") shouldBe true
    tbl(0)("a.aa") shouldBe a [StringValue]
    tbl(0)("a.aa").asInstanceOf[StringValue].value shouldEqual("00")
    tbl(0).exists(_._1 == "a.ab") shouldBe true
    tbl(0)("a.ab") shouldBe a [StringValue]
    tbl(0)("a.ab").asInstanceOf[StringValue].value shouldEqual("01")
    tbl(0).exists(_._1 == "b") shouldBe true
    tbl(0)("b") shouldBe a [StringValue]
    tbl(0)("b").asInstanceOf[StringValue].value shouldEqual("02")

    tbl(1).exists(_._1 == "a") shouldBe true
    tbl(1)("a") shouldBe a [StringValue]
    tbl(1)("a").asInstanceOf[StringValue].value shouldEqual("10")
    tbl(1).exists(_._1 == "b.ba") shouldBe true
    tbl(1)("b.ba") shouldBe a [StringValue]
    tbl(1)("b.ba").asInstanceOf[StringValue].value shouldEqual("11")
    tbl(1).exists(_._1 == "b.bb") shouldBe true
    tbl(1)("b.bb") shouldBe a [StringValue]
    tbl(1)("b.bb").asInstanceOf[StringValue].value shouldEqual("12")
  }
}
