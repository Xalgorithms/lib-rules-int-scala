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

import org.scalamock.scalatest.MockFactory
import org.scalatest._

import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._
import org.xalgorithms.rules.steps._

class ReduceStepSpec extends FlatSpec with Matchers with MockFactory {
  "ReduceStep" should "reduce tables in place" in {
    val ctx = mock[Context]

    val table0 = Seq(
      Map("a" -> new NumberValue(1.0), "b" -> new NumberValue(2.0)),
      Map("a" -> new NumberValue(11.0), "b" -> new NumberValue(12.0)),
      Map("a" -> new NumberValue(21.0), "b" -> new NumberValue(22.0))
    )

    val section = "table"
    val table_name = "table0"

    val step = new ReduceStep(
      Seq(),
      new TableReference(section, table_name),
      Seq(
        new Assignment("c", new DocumentReferenceValue("_context", "b")),
        new Assignment("d", new DocumentReferenceValue("_context", "a")),
        new Assignment("e", new FunctionValue(
          "add", Seq(
            new DocumentReferenceValue("_context", "a"),
            new DocumentReferenceValue("_local", "e")
          )
        ))
      )
    )

    val expected = Seq(
      Map(
        "c" -> new NumberValue(22.0),
        "d" -> new NumberValue(21.0),
        "e" -> new NumberValue(33.0)
      )
    )

    (ctx.lookup_table _).expects(section, table_name).returning(table0)
    (ctx.retain_table _).expects(section, table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(expected.size)
      (tbl, expected).zipped.foreach { case (rac, rex) =>
        rac.keySet shouldEqual(rex.keySet)
        rex.foreach { case (k, v) =>
          rac(k) shouldBe a [NumberValue]
          rac(k).asInstanceOf[NumberValue].value shouldEqual(v.asInstanceOf[NumberValue].value)
        }
      }
    }

    step.execute(ctx)
  }

  it should "only include matching elements when executing" in {
    val ctx = mock[Context]

    val table0 = Seq(
      Map("a" -> new NumberValue(1.0), "b" -> new NumberValue(2.0)),
      Map("a" -> new NumberValue(11.0), "b" -> new NumberValue(12.0)),
      Map("a" -> new NumberValue(16.0), "b" -> new NumberValue(6.0)),
      Map("a" -> new NumberValue(5.0), "b" -> new NumberValue(6.0)),
      Map("a" -> new NumberValue(21.0), "b" -> new NumberValue(22.0))
    )

    val section = "table"
    val table_name = "table0"

    val step = new ReduceStep(
      Seq(
        new When(
          new DocumentReferenceValue("_context", "a"),
          new NumberValue(15.0),
          "lt")
      ),
      new TableReference(section, table_name),
      Seq(
        new Assignment("c", new FunctionValue(
          "add", Seq(
            new DocumentReferenceValue("_context", "a"),
            new DocumentReferenceValue("_local", "c")
          )
        ))
      )
    )

    val expected = Seq(
      Map(
        "c" -> new NumberValue(17.0)
      )
    )

    (ctx.lookup_table _).expects(section, table_name).returning(table0)
    (ctx.retain_table _).expects(section, table_name, *) onCall { (section, name, tbl) =>
      tbl.size shouldEqual(expected.size)
      (tbl, expected).zipped.foreach { case (rac, rex) =>
        rac.keySet shouldEqual(rex.keySet)
        rex.foreach { case (k, v) =>
          rac(k) shouldBe a [NumberValue]
          rac(k).asInstanceOf[NumberValue].value shouldEqual(v.asInstanceOf[NumberValue].value)
        }
      }
    }

    step.execute(ctx)
  }
}
