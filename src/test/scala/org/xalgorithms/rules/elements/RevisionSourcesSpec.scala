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

import org.xalgorithms.rules.{ Change, ChangeOps, Context }

class RevisionSourcesSpec extends FlatSpec with Matchers with MockFactory {
  // val faker = new Faker()

  // class TestTable {
  //   val cols = (0 to faker.number().numberBetween(2, 10)).map { cn => s"column_${cn}" }
  //   val tbl = (0 to faker.number().numberBetween(2, 10)).map { _ =>
  //     cols.map { cn =>
  //       Tuple2(cn, new StringValue(faker.lorem().word()))
  //     }.toMap
  //   }
  // }

  // def sample[T](it: Iterable[T]): T = {
  //   scala.util.Random.shuffle(it).take(1).head
  // }

  // "UpdateRevisionSource" should "produce a list of updates to a table" in {
  //   val table = new TestTable()
  //   val table_ref = new TableReference(faker.lorem.word(), faker.lorem.word())

  //   (0 to faker.number().numberBetween(2, 10)).foreach { _ =>
  //     val ctx = mock[Context]
  //     val column_name = sample(table.cols)
  //     val src = new UpdateRevisionSource(column_name, Seq(), table_ref)

  //     (ctx.lookup_table _).expects(table_ref.section, table_ref.name).returning(table.tbl)
  //     val changes = src.evaluate(ctx)

  //     changes.size shouldEqual(table.tbl.size)
  //     val vals = table.tbl.map { row => row(column_name) }
  //     (changes, vals).zipped.foreach { case (ch, v) =>
  //       ch.exists(_._1 == column_name) shouldBe(true)
  //       ch(column_name).op shouldEqual(ChangeOps.Update)
  //       ch(column_name).value shouldBe a [StringValue]
  //       ch(column_name).value.asInstanceOf[StringValue].value shouldEqual(v.asInstanceOf[StringValue].value)
  //     }
  //   }
  // }

  // "AddRevisionSource" should "produce a list of adds to a table" in {
  //   val table = new TestTable()
  //   val table_ref = new TableReference(faker.lorem.word(), faker.lorem.word())

  //   (0 to faker.number().numberBetween(2, 10)).foreach { _ =>
  //     val ctx = mock[Context]
  //     val column_name = sample(table.cols)
  //     val src = new AddRevisionSource(column_name, Seq(), table_ref)

  //     (ctx.lookup_table _).expects(table_ref.section, table_ref.name).returning(table.tbl)
  //     val changes = src.evaluate(ctx)

  //     changes.size shouldEqual(table.tbl.size)
  //     val vals = table.tbl.map { row => row(column_name) }
  //     (changes, vals).zipped.foreach { case (ch, v) =>
  //       ch.exists(_._1 == column_name) shouldBe(true)
  //       ch(column_name).op shouldEqual(ChangeOps.Add)
  //       ch(column_name).value shouldBe a [StringValue]
  //       ch(column_name).value.asInstanceOf[StringValue].value shouldEqual(v.asInstanceOf[StringValue].value)
  //     }
  //   }
  // }

  // "RemoveRevisionSource" should "produce a list of changes referencing a column" in {
  //   val table = new TestTable()
  //   val table_ref = new TableReference(faker.lorem.word(), faker.lorem.word())

  //   (0 to faker.number().numberBetween(2, 10)).foreach { _ =>
  //     val ctx = mock[Context]
  //     val column_name = sample(table.cols)
  //     val src = new RemoveRevisionSource(column_name, Seq())

  //     val changes = src.evaluate(ctx)

  //     changes.size shouldEqual(1)
  //     changes.head.exists(_._1 == column_name) shouldBe(true)
  //     changes.head(column_name).op shouldEqual(ChangeOps.Remove)
  //     changes.head(column_name).value shouldEqual(null)
  //   }
  // }
}

