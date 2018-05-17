package org.xalgorithms.rules.steps

import org.xalgorithms.rules.elements.{ Assignment, TableReference }

abstract class AssignmentStep(val table: TableReference, val assignments: Seq[Assignment]) extends Step {
}

