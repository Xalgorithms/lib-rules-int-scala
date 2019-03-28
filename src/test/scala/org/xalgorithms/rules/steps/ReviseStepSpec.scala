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
  val faker = new Faker()

  def make_string_table(tbl: Seq[Map[String, String]]): Seq[Map[String, IntrinsicValue]] = {
    tbl.map(make_string_row)
  }

  def make_string_row(row: Map[String, String]): Map[String, IntrinsicValue] = {
    row.mapValues { v => new StringValue(v) }
  }

  def validate_addition(
    target_table: Seq[Map[String, String]],
    changes: Seq[Map[String, String]],
    expected: Seq[Option[Map[String, String]]]
  ): Unit = {
    val ctx = mock[Context]
    val secs = mock[Sections]
    val tables = mock[TableSection]

    val target_ref = new TableReference(faker.hacker().noun())
    val source_ref = new TableReference(faker.hacker().noun())

    val source = mock[RevisionSource]
    val sources = Seq(source)

    val source_evaluation = changes.map { row => new Addition(make_string_row(row)) }

    (source.evaluate _).expects(ctx).returning(source_evaluation)
    (ctx.revise_table _).expects(target_ref, *).once onCall { (_, rev) =>
      rev.changes.size shouldEqual(target_table.size)
      (expected, rev.changes).zipped.foreach { case (opt_ex_row, applied_changes) =>
        applied_changes.size shouldEqual(1)
        val opt_ch = applied_changes.head
        opt_ex_row match {
          case Some(ex_row) => {
            opt_ch match {
              case Some(ch) => {
                ch shouldBe a [Addition]
                val add = ch.asInstanceOf[Addition]

                add.columns.size shouldEqual(ex_row.size)
                ex_row.foreach { case (k, v) =>
                  add.columns.exists(_._1 == k) shouldBe(true)
                  add.columns(k) shouldBe a [StringValue]
                  add.columns(k).asInstanceOf[StringValue].value shouldEqual(v)
                }
              }
              case None => true shouldEqual(false)
            }
          }
          case None => {
            opt_ch shouldBe(None)
          }
        }
      }
    }

    (ctx.sections _).expects().returning(secs)
    (secs.tables _).expects().returning(tables)
    (tables.lookup _).expects(target_ref.name).returning(Some(make_string_table(target_table)))

    val step = new ReviseStep(target_ref, sources)
    step.execute(ctx)
  }

  "ReviseStep" should "generate adds" in {
    val target_table = Seq(
      Map("a" -> "0", "b" -> "2"),
      Map("a" -> "1", "b" -> "4"),
      Map("a" -> "2", "b" -> "6")
    )

    val changes = Seq(
      Map("c" -> "3"),
      Map("c" -> "6"),
      Map("c" -> "9")
    )
    val expected = changes.map { ch => Some(ch) }

    validate_addition(target_table, changes, expected)
  }

  it should "generate empty changes if the source has less rows" in {
    val target_table = Seq(
      Map("a" -> "0", "b" -> "2"),
      Map("a" -> "1", "b" -> "4"),
      Map("a" -> "2", "b" -> "6")
    )

    val changes: Seq[Map[String, String]] = Seq(
      Map("c" -> "3"),
      Map("c" -> "6")
    )

    val expected: Seq[Option[Map[String, String]]] = Seq(
      Some(Map("c" -> "3")),
      Some(Map("c" -> "6")),
      None
    )

    validate_addition(target_table, changes, expected)
  }

  it should "truncate to the size of the target table" in {
    val target_table = Seq(
      Map("a" -> "0", "b" -> "2"),
      Map("a" -> "1", "b" -> "4"),
      Map("a" -> "2", "b" -> "6")
    )

    val changes: Seq[Map[String, String]] = Seq(
      Map("c" -> "3"),
      Map("c" -> "6"),
      Map("c" -> "9"),
      Map("c" -> "12")
    )

    val expected: Seq[Option[Map[String, String]]] = Seq(
      Some(Map("c" -> "3")),
      Some(Map("c" -> "6")),
      Some(Map("c" -> "9"))
    )

    validate_addition(target_table, changes, expected)
  }
}
