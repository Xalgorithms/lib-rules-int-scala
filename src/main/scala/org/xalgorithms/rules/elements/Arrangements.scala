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

abstract class ArrangeFunctionApplication(val val_args: Seq[Value]) {
  def resolve_args(ctx: Context): Seq[Int] = {
    ResolveManyValues(val_args, ctx).foldLeft(Seq(): Seq[Int]) { case (seq, iv_opt) =>
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

  def arrange(
    ctx: Context,
    tbl: Seq[Map[String, IntrinsicValue]]
  ): Seq[Map[String, IntrinsicValue]] = {
    arrange(tbl, resolve_args(ctx))
  }

  def arrange(
    tbl: Seq[Map[String, IntrinsicValue]],
    args: Seq[Int]
  ): Seq[Map[String, IntrinsicValue]]
}

class InvertFunctionApplication(val_args: Seq[Value]) extends ArrangeFunctionApplication(val_args) {
  def arrange(
    tbl: Seq[Map[String, IntrinsicValue]],
    args: Seq[Int]
  ): Seq[Map[String, IntrinsicValue]] = {
    Seq()
  }
}

class ShiftFunctionApplication(val_args: Seq[Value]) extends ArrangeFunctionApplication(val_args) {
  def arrange(
    tbl: Seq[Map[String, IntrinsicValue]],
    args: Seq[Int]
  ): Seq[Map[String, IntrinsicValue]] = {
    if (args.length > 0 && args(0) != 0) {
      val len = tbl.length
      val dist = args(0)
      if (dist > 0) {
        // right
        val (h, t) = tbl.splitAt(len - (dist % len))
        t ++ h
      } else {
        // left
        val (h, t) = tbl.splitAt(Math.abs(dist) % len)
        t ++ h
      }
    } else {
      tbl
    }
  }
}

class SortFunctionApplication(val_args: Seq[Value]) extends ArrangeFunctionApplication(val_args) {
  def arrange(
    tbl: Seq[Map[String, IntrinsicValue]],
    args: Seq[Int]
  ): Seq[Map[String, IntrinsicValue]] = {
    Seq()
  }
}

class ArrangeFunction(val name: String, val args: Seq[Value] = Seq()) {
  val _app_opt = name match {
    case "invert" => Some(new InvertFunctionApplication(args))
    case "shift" => Some(new ShiftFunctionApplication(args))
    case "sort" => Some(new SortFunctionApplication(args))
    case _ => None
  }

  def arrange(
    ctx: Context,
    tbl: Seq[Map[String, IntrinsicValue]]
  ): Seq[Map[String, IntrinsicValue]] = {
    _app_opt.map(_.arrange(ctx, tbl)).getOrElse(Seq[Map[String, IntrinsicValue]]())
  }
}

class Arrangement(val func: ArrangeFunction) {
  def arrange(
    ctx: Context,
    tbl: Seq[Map[String, IntrinsicValue]]
  ): Seq[Map[String, IntrinsicValue]] = {
    func.arrange(ctx, tbl)
  }
}
