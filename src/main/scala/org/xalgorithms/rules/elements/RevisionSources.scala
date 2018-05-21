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

import org.xalgorithms.rules.{ Change, ChangeOps, Context }

abstract class RevisionSource(val column: String, val whens: Seq[When]) {
  def evaluate(ctx: Context): Seq[Map[String, Change]]
}

class TableRevisionSource(
  column: String,
  whens: Seq[When],
  val table: TableReference,
  val op: ChangeOps.Value
) extends RevisionSource(column, whens) {
  def evaluate(ctx: Context): Seq[Map[String, Change]] = {
    ctx.lookup_table(table.section, table.name).map { r =>
      Map(column -> new Change(op, r(column)))
    }
  }
}

class AddRevisionSource(
  column: String,
  whens: Seq[When],
  table: TableReference
) extends TableRevisionSource(column, whens, table, ChangeOps.Add) {
}

class UpdateRevisionSource(
  column: String,
  whens: Seq[When],
  table: TableReference
) extends TableRevisionSource(column, whens, table, ChangeOps.Update) {
}

class RemoveRevisionSource(column: String, whens: Seq[When]) extends RevisionSource(column, whens) {
  def evaluate(ctx: Context): Seq[Map[String, Change]] = {
    Seq(Map(column -> new Change(ChangeOps.Remove, null)))
  }
}
