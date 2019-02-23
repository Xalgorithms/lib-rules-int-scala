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

import org.xalgorithms.rules.{ Context, RowContext }
import scala.collection.mutable.{ ListBuffer }

abstract class Refinement {
  def refine(): Seq[Map[String, IntrinsicValue]]
  def add(ctx: Context, row: Map[String, IntrinsicValue]): Unit
}

abstract class AccumulateRefinement extends Refinement {
  val _rows = ListBuffer[Map[String, IntrinsicValue]]()

  def add(
    ctx: Context,
    row: Map[String, IntrinsicValue]
  ): Unit = { _rows += row }

  def refine(): Seq[Map[String, IntrinsicValue]] = _rows.toSeq
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

class FilterRefinement(val when: Option[When]) extends AccumulateRefinement {
  override def add(
    ctx: Context,
    row: Map[String, IntrinsicValue]
  ): Unit = Filter(ctx, row, when) match {
    case Some(r) => super.add(ctx, row)
    case None => {}
  }
}

class MapRefinement(val assignment: Option[Assignment]) extends AccumulateRefinement {
  override def add(ctx: Context, row: Map[String, IntrinsicValue]): Unit = {
    super.add(ctx, assignment match {
      case Some(ass) => row ++ ass.evaluate(ctx)
      case None => row
    })
  }
}

abstract class TakeRefinement extends AccumulateRefinement {
}

class TakeFunction(val name: String, val args: Seq[Value] = Seq()) {
  val _args = ResolveManyValues(args, null).foldLeft(Seq(): Seq[Int]) { case (seq, iv_opt) =>
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

  def refine(
    tbl: Seq[Map[String, IntrinsicValue]]
  ): Seq[Map[String, IntrinsicValue]] = name match {
    case "first" => refine_first(tbl)
    case "last"  => refine_last(tbl)
    case "nth"   => refine_nth(tbl)
    case _       => Seq()
  }

  def refine_first(tbl: Seq[Map[String, IntrinsicValue]]): Seq[Map[String, IntrinsicValue]] = {
    if (_args.length > 0) {
      tbl.slice(0, _args(0))
    } else {
      Seq()
    }
  }

  def refine_last(tbl: Seq[Map[String, IntrinsicValue]]): Seq[Map[String, IntrinsicValue]] = {
    if (_args.length > 0) {
      tbl.slice(tbl.length - _args(0), tbl.length)
    } else {
      Seq()
    }
  }

  def refine_nth(tbl: Seq[Map[String, IntrinsicValue]]): Seq[Map[String, IntrinsicValue]] = {
    if (_args.length > 1) {
      tbl.slice(_args(0), _args(1) + 1)
    } else {
      Seq()
    }
  }
}

class ConditionalTakeRefinement(val when: Option[When]) extends TakeRefinement {
  override def add(
    ctx: Context,
    row: Map[String, IntrinsicValue]
  ): Unit = Filter(ctx, row, when) match {
    case Some(r) => super.add(ctx, row)
    case None => {}
  }
}

class FunctionalTakeRefinement(val func: Option[TakeFunction]) extends TakeRefinement {
  override def refine(): Seq[Map[String, IntrinsicValue]] = func match {
    case Some(fn) => fn.refine(_rows)
    case None => Seq()
  }
}
