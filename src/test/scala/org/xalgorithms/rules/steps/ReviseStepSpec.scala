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

import com.github.javafaker.Faker
import org.scalamock.scalatest.MockFactory
import org.scalatest._

import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._
import org.xalgorithms.rules.steps._

class ReviseStepSpec extends FlatSpec with Matchers with MockFactory {
  "ReviseStep" should "evaluate RevisionSources" in {
    val ctx = mock[Context]
    val faker = new Faker()
    val change_ops = Seq(
      Tuple2(faker.crypto().sha1(), ChangeOps.Add),
      Tuple2(faker.crypto().sha1(), ChangeOps.Remove),
      Tuple2(faker.crypto().sha1(), ChangeOps.Update)
    )
    val srcs = change_ops.map { tup =>
      val src = mock[RevisionSource]
      (src.evaluate _).expects(ctx).returning(
        Seq(Map(tup._1 -> new Change(tup._2, null))))
      src
    }
    val table_ref = new TableReference(faker.hacker().noun(), faker.hacker().noun())

    (ctx.add_revision _).expects(table_ref.name, *) onCall { (name, rev) =>
      // the way we're simulating in the RevisionSource mocks, we're
      // acting as if the table has one row
      rev.changes.size shouldEqual(1)
      rev.changes.head.size shouldEqual(change_ops.size)
      change_ops.foreach { ch_op =>
        rev.changes.head.exists(_._1 == ch_op._1) shouldEqual(true)
        rev.changes.head(ch_op._1).op shouldEqual(ch_op._2)
        rev.changes.head(ch_op._1).value shouldBe(null)
      }
    }

    val step = new ReviseStep(table_ref, srcs)
    step.execute(ctx)
  }
}
