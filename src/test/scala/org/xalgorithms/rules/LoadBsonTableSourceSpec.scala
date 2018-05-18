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
package org.xalgorithms.rules

import org.bson.{ BsonArray, BsonDocument }
import org.scalatest._
import scala.io.Source

import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._

class LoadJsonTableSourceSpec extends FlatSpec with Matchers {
  class ResourceLoadBsonSource extends LoadBsonTableSource {
    def read(ptref: PackagedTableReference): BsonArray = {
      BsonArray.parse(Source.fromURL(getClass.getResource(s"/${ptref.name}.json")).mkString)
    }
  }

  "LoadJsonTableSourceSpec" should "convert BsonDocuments to internal tables" in {
    val loader = new ResourceLoadBsonSource()

    val tbl0 = loader.load(new PackagedTableReference("table", "table0", "0.0.1", "table0"))

    tbl0.length shouldEqual(3)

    tbl0(0).exists(_._1 == "a") shouldBe true
    tbl0(0)("a") shouldBe a [NumberValue]
    tbl0(0)("a").asInstanceOf[NumberValue].value shouldEqual(1.0)

    tbl0(0).exists(_._1 == "b") shouldBe true
    tbl0(0)("b") shouldBe a [StringValue]
    tbl0(0)("b").asInstanceOf[StringValue].value shouldEqual("foo")

    tbl0(1).exists(_._1 == "a") shouldBe true
    tbl0(1)("a") shouldBe a [NumberValue]
    tbl0(1)("a").asInstanceOf[NumberValue].value shouldEqual(2.0)

    tbl0(1).exists(_._1 == "b") shouldBe true
    tbl0(1)("b") shouldBe a [StringValue]
    tbl0(1)("b").asInstanceOf[StringValue].value shouldEqual("bar")

    tbl0(2).exists(_._1 == "a") shouldBe true
    tbl0(2)("a") shouldBe a [NumberValue]
    tbl0(2)("a").asInstanceOf[NumberValue].value shouldEqual(3.0)

    tbl0(2).exists(_._1 == "b") shouldBe true
    tbl0(2)("b") shouldBe a [StringValue]
    tbl0(2)("b").asInstanceOf[StringValue].value shouldEqual("baz")
  }
}
