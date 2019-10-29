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
package org.xalgorithms.rules.elements

import com.github.javafaker.Faker
import org.scalamock.scalatest.MockFactory
import org.scalatest._
import play.api.libs.json._

import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._

class ContextSpec extends FlatSpec with Matchers with MockFactory {
  val faker = new Faker()

  "GlobalContext" should "record revisions to a table" in {
    val ctx = new GlobalContext(null)
    val tables = (0 to faker.number().numberBetween(2, 10)).map { i =>
      new TableReference(s"table${i}")
    }

    tables.foreach { table => ctx.revise_table(table, new Revision(Seq())) }
    tables.foreach { table => ctx.revise_table(table, new Revision(Seq())) }

    val revs = ctx.revisions()
    revs.size shouldEqual(tables.size)
    tables.foreach { table =>
      revs(table).size shouldEqual(2)
    }
  }

  it should "serialize into JSON" in {
    val ctx = new GlobalContext(null)
    val values0 = Map("A" -> 100.0, "B" -> 333.0)
    val table0 = Seq(
        Map("A" -> 2.0, "B" -> 3.0),
        Map("A" -> 4.0, "B" -> 6.0)
    )

    val expected = Json.obj(
      "values" -> Json.obj(
        "values0" -> values0,
      ),
      "tables" -> Json.obj("table0" -> table0),
    )

    ctx.sections.tables.retain("table0", table0.map { r => r.mapValues(new NumberValue(_)) })
    ctx.sections.retain_values("values0", values0.mapValues(new NumberValue(_)))

    ctx.serialize shouldEqual(expected)
  }
}
