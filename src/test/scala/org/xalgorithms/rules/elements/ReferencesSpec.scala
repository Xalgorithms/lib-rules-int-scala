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

import org.scalamock.scalatest.MockFactory
import org.scalatest._

import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._

class ReferencesSpec extends FlatSpec with Matchers with MockFactory {
  def map_to_expected(m: Map[String, String]): Map[String, IntrinsicValue] = {
    m.map { case (k, v) => (k, new StringValue(v)) }
  }

  "TableReference" should "load tables from the Context" in {
    val tables = Map(
      "map0" -> Seq(
        Map("a" -> "00", "b" -> "01"),
        Map("a" -> "10", "b" -> "11")),
      "map1" -> Seq(
        Map("A" -> "xx", "B" -> "yy"),
        Map("A" -> "yy", "B" -> "zz")))
    val ctx = mock[Context]
    val secs = mock[Sections]
    val tables_sec = mock[TableSection]

    (ctx.sections _).expects().repeated(tables.size).times.returning(secs)
    (secs.tables _).expects().repeated(tables.size).times.returning(tables_sec)

    tables.foreach { case (key, ex) =>
      val expected = ex.map(map_to_expected)
      val ref = new TableReference(key)

      (tables_sec.lookup _).expects(key).returning(Some(expected))
      ref.get(ctx) shouldEqual(Some(expected))
    }
  }
}
