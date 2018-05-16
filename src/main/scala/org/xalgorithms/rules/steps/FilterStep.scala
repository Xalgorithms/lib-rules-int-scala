package org.xalgorithms.rules.steps

import org.xalgorithms.rules.{ Context, RowContext }
import org.xalgorithms.rules.elements.{ EvaluateMany, ReferenceValue, TableReference, When }

class FilterStep(val table: TableReference, val filters: Seq[When]) extends Step {
  def execute(ctx: Context) {
    val tbl = ctx.lookup_table(table.section, table.name)

    ctx.retain_table(table.section, table.name, tbl.filter { r =>
      EvaluateMany(new RowContext(ctx, Map(), r), filters)
    })
  }
}

