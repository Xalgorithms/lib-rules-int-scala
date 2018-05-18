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

import org.xalgorithms.rules.{ Context, Change, ChangeOps, Revision }
import org.xalgorithms.rules.elements.{ RevisionSource, TableReference, UpdateRevisionSource, Value }

class ReviseStep(val table: TableReference, val revisions: Seq[RevisionSource]) extends Step {
  def execute(ctx: Context) {
    val all_changes = revisions.foldLeft(Seq[Map[String, Change]]()) { (seq, src) =>
      val changes = changes_from_source(ctx, src)
      seq.zipAll(changes, Map(), Map()).map { tup => tup._1 ++ tup._2 }
    }

    ctx.add_revision(table.name, new Revision(all_changes))
  }

  def changes_from_source(ctx: Context, src: RevisionSource): Seq[Map[String, Change]] = src match {
    // DEBT: Missing Add/Remove
    case (urs: UpdateRevisionSource) => changes_from_update(ctx, urs)
    case _ => Seq[Map[String, Change]]()
  }

  def changes_from_update(ctx: Context, src: UpdateRevisionSource): Seq[Map[String, Change]] = {
    val tbl = ctx.lookup_table(src.table.section, src.table.name)
    tbl.map { r => Map(src.column -> new Change(ChangeOps.Update, r(src.column))) }
  }
}

