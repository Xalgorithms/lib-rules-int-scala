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

