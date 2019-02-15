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
package org.xalgorithms.rules.elements

import org.xalgorithms.rules.{ Context }

abstract class Refinement {
  def refine(ctx: Context, row: Map[String, IntrinsicValue]): Option[Map[String, IntrinsicValue]]
}

class FilterRefinement(val when: Option[When]) extends Refinement {
  def refine(
    ctx: Context,
    row: Map[String, IntrinsicValue]
  ): Option[Map[String, IntrinsicValue]] = when match {
    case Some(wh) => wh.evaluate(ctx) match {
      case true => Some(row)
      case false => None
    }
    case None => None
  }
}

class MapRefinement(val assignment: Option[Assignment]) extends Refinement {
  def refine(
    ctx: Context,
    row: Map[String, IntrinsicValue]
  ): Option[Map[String, IntrinsicValue]] = assignment match {
    case Some(a) => Some(row ++ a.evaluate(ctx))
    case None => Some(row)
  }
}

abstract class TakeRefinement extends Refinement {
}

class ConditionalTakeRefinement(val when: Option[When]) extends TakeRefinement {
  def refine(ctx: Context, row: Map[String, IntrinsicValue]): Option[Map[String, IntrinsicValue]] = {
    None
  }
}

// The FunctionValue should contain a function that is a predicate that tests
// whether this row's index is within a range. This will require an init
// function ONLY FOR THIS Refinement that hints at the size of the table.
class FunctionalTakeRefinement(val func: Option[FunctionValue]) extends TakeRefinement {
  def refine(ctx: Context, row: Map[String, IntrinsicValue]): Option[Map[String, IntrinsicValue]] = {
    None
  }
}
