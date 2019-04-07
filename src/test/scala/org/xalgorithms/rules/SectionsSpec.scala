// Copyright (C) 2018-2019 Don Kelly <karfai@gmail.com>
// Copyright (C) 2018-2019 Hayk Pilosyan <hayk.pilos@gmail.com>

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
import play.api.libs.json._

class SectionsSpec extends FlatSpec with Matchers with MockFactory {
  "ValuesSection" should "contain nothing by default" in {
    val sec = new ValuesSection()

    sec.lookup("a") shouldEqual(None)
    sec.lookup("a.b") shouldEqual(None)
    sec.lookup("a.c") shouldEqual(None)

    val vals = Map("a" -> 1, "a.b" -> 2, "a.c" -> 3)

    vals.foreach { case (k, v) => sec.retain(k, new NumberValue(v)) }
    vals.foreach { case (k, v) =>
      sec.lookup(k) match {
        case Some(iv) => {
          iv shouldBe a [NumberValue]
          iv.asInstanceOf[NumberValue].value shouldEqual(v)
        }
        case None => true shouldEqual(false)
      }
    }
  }

  it should "accept a Map as initializer" in {
    val vals = Map("a" -> 1, "a.b" -> 2, "a.c" -> 3)

    val sec = new ValuesSection(Some(vals.mapValues(new NumberValue(_))))

    vals.foreach { case (k, v) =>
      sec.lookup(k) match {
        case Some(iv) => {
          iv shouldBe a [NumberValue]
          iv.asInstanceOf[NumberValue].value shouldEqual(v)
        }
        case None => true shouldEqual(false)
      }
    }
  }

  it should "serialize to JSON" in {
    val vals = Map("a" -> 1, "a.b" -> 2, "a.c" -> 3)

    val sec = new ValuesSection(Some(vals.mapValues(new NumberValue(_))))
    sec.serialize shouldEqual(Json.toJson(vals))
  }

  def verify_table(
    ac_tbl: Seq[Map[String, IntrinsicValue]],
    ex_tbl: Seq[Map[String, IntrinsicValue]]
  ) = {
    ac_tbl.size shouldEqual(ex_tbl.size)
    ac_tbl.zipWithIndex.foreach { case (ac_row, i) =>
      val ex_row = ex_tbl(i)
      ac_row.size shouldEqual(ex_row.size)
      ac_row.foreach { case (ck, ac_iv) =>
        ex_row.get(ck) match {
          case Some(ex_iv) => {
            ac_iv shouldBe a [StringValue]
            ex_iv shouldBe a [StringValue]
            ac_iv.asInstanceOf[StringValue].value shouldEqual(ex_iv.asInstanceOf[StringValue].value)
          }
          case None => true shouldBe(false)
        }
      }
    }
  }

  "TableSection" should "retain tables" in {
    val sec = new TableSection()
    val table0 = Seq(
      Map("x" -> "00", "y" -> "01"),
      Map("x" -> "10", "y" -> "11")
    ).map { r => r.mapValues(new StringValue(_)) }
    val table1 = Seq(
      Map("p" -> "00", "q" -> "01"),
      Map("p" -> "10", "q" -> "11")
    ).map { r => r.mapValues(new StringValue(_)) }

    val tables = Map("a" -> table0, "b" -> table1)

    tables.foreach { case (k, tbl) =>
      sec.lookup(k) shouldEqual(None)
      sec.retain(k, tbl)
      sec.lookup(k) match {
        case Some(ac_tbl) => verify_table(ac_tbl, tbl)
        case None => true shouldBe(false)
      }
    }
  }

  it should "load tables on demand" in {
    val source = mock[LoadTableSource]
    val sec = new TableSection(Some(source))
    val ptref = new PackagedTableReference("package", "id", "0.0.1", "a")

    val table0 = Seq(
      Map("x" -> "00", "y" -> "01"),
      Map("x" -> "10", "y" -> "11")
    ).map { r => r.mapValues(new StringValue(_)) }

    (source.load _).expects(ptref).once.returning(table0)

    sec.lookup(ptref.name) shouldEqual(None)

    sec.remember(ptref)
    sec.lookup(ptref.name) match {
      case Some(ac_tbl) => verify_table(ac_tbl, table0)
      case None => true shouldBe(false)
    }
    sec.lookup(ptref.name) match {
      case Some(ac_tbl) => verify_table(ac_tbl, table0)
      case None => true shouldBe(false)
    }
  }

  it should "serialize to JSON" in {
    val tables = Map(
      "table0" -> Seq(
        Map("x" -> "00", "y" -> "01"),
        Map("x" -> "10", "y" -> "11")
      ),
      "table1" -> Seq(
        Map("p" -> "00", "q" -> "01"),
        Map("p" -> "10", "q" -> "11")
      )
    )

    val sec = new TableSection
    tables.foreach { case (k, tbl) =>
      sec.retain(k, tbl.map { r => r.mapValues(new StringValue(_)) })
    }

    sec.serialize shouldEqual(Json.toJson(tables))
  }

  "Sections" should "serialize to JSON" in {
    val secs = new Sections()
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

    secs.tables.retain("table0", table0.map { r => r.mapValues(new NumberValue(_)) })
    secs.retain_values("values0", values0.mapValues(new NumberValue(_)))

    secs.serialize shouldEqual(expected)
  }

  it should "retain values" in {
    val values = Map(
      "a" -> new StringValue("A"),
      "b" -> new StringValue("B"),
    )

    val secs = new Sections()
    secs.retain_values("values", values)
    secs.unload_values("values")
    secs.values("values") shouldEqual(None)
  }
}
