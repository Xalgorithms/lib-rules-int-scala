// Copyright 2018 Don Kelly <karfai@gmail.com>

// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License. You may
// obtain a copy of the License at

// http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied. See the License for the specific language governing
// permissions and limitations under the License.

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

