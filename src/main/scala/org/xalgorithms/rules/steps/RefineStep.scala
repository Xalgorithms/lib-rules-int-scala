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

import org.xalgorithms.rules.{ Context }
import org.xalgorithms.rules.elements._

class RefineStep(
  val table: TableReference,
  val refined_name: String,
  val refinements: Seq[Refinement]
) extends Step {
  val _grouped = refinements.groupBy[String] { r =>
    r match {
      case fr: FilterRefinement => "filter"
      case mr: MapRefinement    => "map"
      case tr: TakeRefinement   => "take"
      case _                    => "unknown"
    }
  }

  val _application_order = Seq("filter", "map", "take")

  def apply_refinements(
    ctx: Context,
    k: String,
    row_opt: Option[Map[String, IntrinsicValue]]
  ): Option[Map[String, IntrinsicValue]] = {
    _grouped.getOrElse(k, Seq()).foldLeft(row_opt) { case (refined_row_opt, refinement) =>
      refined_row_opt match {
        case Some(refined_row) => refinement.refine(ctx, refined_row)
        case None => None
      }
    }
  }

  def execute(ctx: Context) {
    val ftbl = ctx.lookup_table(
      table.section,
      table.name
    ).foldLeft(Seq[Map[String, IntrinsicValue]]()) { case (ntbl, row) =>
        val original_row_opt: Option[Map[String, IntrinsicValue]] = Some(row)
        _application_order.foldLeft(original_row_opt) { case (row_opt, k) =>
          apply_refinements(ctx, k, row_opt)
        } match {
          case Some(final_row) => ntbl ++ Seq(final_row)
          case None => ntbl
        }
    }

    ctx.retain_table(table.section, refined_name, ftbl)
  }
}
