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

