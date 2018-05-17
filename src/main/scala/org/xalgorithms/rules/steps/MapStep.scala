// Copyright (C) 2018 Don Kelly <karfai@gmail.com>
// Copyright (C) 2018 Hayk Pilosyan <hayk.pilos@gmail.com>

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
package org.xalgorithms.rules.steps

import org.xalgorithms.rules.{ Context, RowContext }
import org.xalgorithms.rules.elements._

class MapStep(table: TableReference, assignments: Seq[Assignment]) extends AssignmentStep(table, assignments) {
  def execute(ctx: Context) {
    val tbl = ctx.lookup_table(table.section, table.name)
    ctx.retain_table(table.section, table.name, tbl.map { row =>
      val rctx = new RowContext(ctx, row, row)
      assignments.foldLeft(row) { (o, ass) =>
        val ch: Map[String, IntrinsicValue] = ResolveValue(ass.source, rctx) match {
          case Some(av) => Map(ass.target -> av)
          case None => Map()
        }
        rctx.update_local(ch)
        o ++ ch
      }
    })
  }
}

