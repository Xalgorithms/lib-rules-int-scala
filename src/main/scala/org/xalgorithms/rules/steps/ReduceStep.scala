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

import org.xalgorithms.rules.{ Context, RowContext }
import org.xalgorithms.rules.elements._

class ReduceStep(
  val filters: Seq[When],
  table: TableReference,
  assignments: Seq[Assignment]
) extends AssignmentStep(table, assignments) {
  def execute(ctx: Context) {
    val tbl = ctx.lookup_table(table.section, table.name)
    val reduced = tbl.foldLeft(Map[String, IntrinsicValue]()) { (acc_row, row) =>
      val rctx = new RowContext(ctx, acc_row, row)
      // DEBT: This isn't quite correct b/c if there are multiple
      // assignments against the row, this WHEN check won't be able to
      // access previous local assignments
      if (EvaluateMany(rctx, filters)) {
        assignments.foldLeft(Map[String, IntrinsicValue]()) { (nrow, ass) =>
          val ch: Map[String, IntrinsicValue] = ResolveValue(ass.source, rctx) match {
            case Some(av) => Map(ass.target -> av)
            case None => Map()
          }
          rctx.update_local(ch)
          nrow ++ ch
        }
      } else {
        acc_row
      }
    }

    ctx.retain_table(table.section, table.name, Seq(reduced))
  }
}

