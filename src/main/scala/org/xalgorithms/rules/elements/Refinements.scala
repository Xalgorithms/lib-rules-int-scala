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

object Filter {
  def apply(
    ctx: Context,
    row: Map[String, IntrinsicValue],
    when: Option[When]
  ): Option[Map[String, IntrinsicValue]] = when match {
    case Some(wh) => wh.evaluate(ctx) match {
      case true => Some(row)
      case false => None
    }
    case None => None
  }
}

class FilterRefinement(val when: Option[When]) extends Refinement {
  def refine(
    ctx: Context,
    row: Map[String, IntrinsicValue]
  ) = Filter(ctx, row, when)
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

class TakeFunction(val name: String, val args: Seq[Value] = Seq()) {
}

class ConditionalTakeRefinement(val when: Option[When]) extends TakeRefinement {
  def refine(
    ctx: Context,
    row: Map[String, IntrinsicValue]
  ) = Filter(ctx, row, when)
}

// The FunctionValue should contain a function that is a predicate that tests
// whether this row's index is within a range. This will require an init
// function ONLY FOR THIS Refinement that hints at the size of the table.
class FunctionalTakeRefinement(val func: Option[TakeFunction]) extends TakeRefinement {
  var _index = 0

  def refine(ctx: Context, row: Map[String, IntrinsicValue]): Option[Map[String, IntrinsicValue]] = func match {
    case Some(fn) => {
      fn.name match {
        case "first" => refine_first(ctx, row, fn.args)
        case "nth" => refine_nth(ctx, row, fn.args)
        case _ => Some(row)
      }
    }
    case None => None
  }

  def args_as_ints(ctx: Context, args: Seq[Value]): Seq[Int] = {
    ResolveManyValues(args, ctx).foldLeft(Seq(): Seq[Int]) { case (seq, iv_opt) =>
      iv_opt match {
        case Some(iv) => {
          try {
            iv match {
              case (sv: StringValue) => seq :+ sv.value.toInt
              case (nv: NumberValue) => seq :+ nv.value.toInt
              case _ => seq
            }
          } catch {
            case _: Throwable => seq
          }
        }
        case None => seq
      }
    }
  }

  def refine_by_range(
    range: Range,
    row: Map[String, IntrinsicValue]
  ): Option[Map[String, IntrinsicValue]] = {
    val rv = if (range.contains(_index)) {
      Some(row)
    } else {
      None
    }

    _index += 1
    rv
  }

  def refine_first(
    ctx: Context,
    row: Map[String, IntrinsicValue],
    args: Seq[Value]
  ): Option[Map[String, IntrinsicValue]] = {
    val iargs = args_as_ints(ctx, args)
    if (iargs.length > 0) {
      refine_by_range((0 to iargs(0) - 1), row)
    } else {
      None
    }
  }

  def refine_nth(
    ctx: Context,
    row: Map[String, IntrinsicValue],
    args: Seq[Value]
  ): Option[Map[String, IntrinsicValue]] = {
    val iargs = args_as_ints(ctx, args)
    if (iargs.length > 1) {
      refine_by_range((iargs(0) to iargs(0) + iargs(1) - 1), row)
    } else {
      None
    }
  }
}
