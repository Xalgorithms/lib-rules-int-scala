package org.xalgorithms.rules.steps

import org.xalgorithms.rules.{ Context }
import org.xalgorithms.rules.elements.{ Assignment, TableReference, When }

class ReduceStep(
  val filters: Seq[When], table: TableReference,
  assignments: Seq[Assignment]) extends AssignmentStep(table, assignments) {
  def execute(ctx: Context) {
  }
}

