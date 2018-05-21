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

class ReviseStepSpec extends FlatSpec with Matchers with MockFactory {
  "ReviseStep" should "produce update Changes for existing tables from updated tables (same row count)" in {
    val ctx = mock[Context]

    val table0 = Seq(
      Map("a" -> new StringValue("a0"), "b" -> new StringValue("foo"), "c" -> new StringValue("c0")),
      Map("a" -> new StringValue("a1"), "b" -> new StringValue("bar"), "c" -> new StringValue("c1")),
      Map("a" -> new StringValue("a2"), "b" -> new StringValue("baz"), "c" -> new StringValue("c2"))
    )
    val table_updates = Seq(
      Map("a" -> new StringValue("AA"), "b" -> new StringValue("FOO")),
      Map("a" -> new StringValue("AB"), "b" -> new StringValue("BAR")),
      Map("a" -> new StringValue("AC"), "b" -> new StringValue("BAZ"))
    )
    val updated_keys = Seq("a", "b")
    val retained_keys = Seq("c")
    val table_name = "table0"
    val table_update_name = "table_updates"
    val table_update_ref = new TableReference("table", table_update_name)

    val step = new ReviseStep(
      new TableReference("table", table_name),
      updated_keys.map { k => new UpdateRevisionSource(k, Seq(), table_update_ref) }
    )

    (ctx.lookup_table _)
      .expects("table", table_update_name)
      .repeat(updated_keys.size)
      .returning(table_updates)
    (ctx.add_revision _).expects(table_name, *) onCall { (name, rev) =>
      rev.changes should not be null
      rev.changes.size shouldEqual(table_updates.size)
      (rev.changes, table_updates).zipped.foreach { case (ch, row) =>
        updated_keys.foreach { k =>
          ch.exists(_._1 == k) shouldEqual(true)
          ch(k).op shouldEqual(ChangeOps.Update)
          ch(k).value shouldBe a [StringValue]
          ch(k).value.asInstanceOf[StringValue].value shouldEqual(row(k).asInstanceOf[StringValue].value)
        }

        retained_keys.foreach { k => ch.exists(_._1 == k) shouldEqual(false) }
      }
    }

    step.execute(ctx)
  }
}
