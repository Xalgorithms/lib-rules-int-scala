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

abstract class ArrangeFunctionApplication(val val_args: Seq[Value]) {
  def arrange(
    ctx: Context,
    tbl: Seq[Map[String, IntrinsicValue]]
  ): Seq[Map[String, IntrinsicValue]]
}

class InvertFunctionApplication(val_args: Seq[Value]) extends ArrangeFunctionApplication(val_args) {
  def arrange(
    ctx: Context,
    tbl: Seq[Map[String, IntrinsicValue]]
  ): Seq[Map[String, IntrinsicValue]] = tbl.reverse
}

class ShiftFunctionApplication(val_args: Seq[Value]) extends ArrangeFunctionApplication(val_args) {
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

abstract class Comparator {
  def compare(a: Option[IntrinsicValue], b: Option[IntrinsicValue]): Boolean
}

abstract class AlphaComparator extends Comparator {
  def compare(a: Option[IntrinsicValue], b: Option[IntrinsicValue]): Boolean = {
    compare_vals(string(a), string(b))
  }

  def string(iv_opt: Option[IntrinsicValue]): Option[String] = iv_opt.map { iv =>
    iv match {
      case (sv: StringValue) => sv.value
      case (nv: NumberValue) => nv.value.toString
      case _ => ""
    }
  }

  def compare_vals(a: Option[String], b: Option[String]): Boolean
}

class AscendingAlphaComparator extends AlphaComparator {
  def compare_vals(a: Option[String], b: Option[String]): Boolean = a match {
    case Some(av) => b match {
      case Some(bv) => av < bv
      case None => false
    }
    case None => false
  }
}

class DescendingAlphaComparator extends AlphaComparator {
  def compare_vals(a: Option[String], b: Option[String]): Boolean = a match {
    case Some(av) => b match {
      case Some(bv) => av > bv
      case None => true
    }
    case None => true
  }
}

abstract class NumericComparator extends Comparator {
  def compare(a: Option[IntrinsicValue], b: Option[IntrinsicValue]): Boolean = {
    compare_vals(number(a), number(b))
  }

  def number(iv_opt: Option[IntrinsicValue]): Option[BigDecimal] = iv_opt.map { iv =>
    iv match {
      case (sv: StringValue) => sv.value.toDouble
      case (nv: NumberValue) => nv.value
      case _ => BigDecimal(0.0)
    }
  }

  def compare_vals(a: Option[BigDecimal], b: Option[BigDecimal]): Boolean
}

class AscendingNumericComparator extends NumericComparator {
  def compare_vals(a: Option[BigDecimal], b: Option[BigDecimal]): Boolean = a match {
    case Some(av) => b match {
      case Some(bv) => av < bv
      case None => false
    }
    case None => false
  }
}

class DescendingNumericComparator extends NumericComparator {
  def compare_vals(a: Option[BigDecimal], b: Option[BigDecimal]): Boolean = a match {
    case Some(av) => b match {
      case Some(bv) => av > bv
      case None => true
    }
    case None => true
  }
}

class SortFunctionApplication(val_args: Seq[Value]) extends ArrangeFunctionApplication(val_args) {
  def arrange(
    ctx: Context,
    tbl: Seq[Map[String, IntrinsicValue]]
  ): Seq[Map[String, IntrinsicValue]] = {
    // val_args: (doc_ref, string, string)
    if (val_args.length > 0) {
      val col_ref = val_args(0)
      val style = if (val_args.length > 1) {
        resolve_string(ctx, val_args(1), "alpha")
      } else {
        "alpha"
      }
      val order = if (val_args.length > 2) {
        resolve_string(ctx, val_args(2), "ascending")
      } else {
        "ascending"
      }
      apply_sort(ctx, tbl, col_ref, lookup_comparator(style, order))
    } else {
      tbl
    }
  }

  def lookup_comparator(
    style: String,
    order: String
  ): Option[Comparator] = style match {
    case "alpha" => order match {
      case "descending" => Some(new DescendingAlphaComparator())
      case _ => Some(new AscendingAlphaComparator())
    }
    case "numeric" => order match {
      case "descending" => Some(new DescendingNumericComparator())
      case _ => Some(new AscendingNumericComparator())
    }
    case _ => None
  }

  def resolve_string(ctx: Context, v: Value, dflt: String): String = {
    ResolveValue(v, ctx) match {
      case Some(iv) => iv match {
        case (sv: StringValue) => sv.value
        case _ => dflt
      }
      case None => dflt
    }
  }

  def apply_sort(
    ctx: Context,
    tbl: Seq[Map[String, IntrinsicValue]],
    ref: Value,
    comparator_opt: Option[Comparator]
  ): Seq[Map[String, IntrinsicValue]] = comparator_opt match {
    case Some(comparator) => {
      val fn = (a: Map[String, IntrinsicValue], b: Map[String, IntrinsicValue]) => {
        comparator.compare(
          ResolveValue(ref, new RowContext(ctx, a, null)),
          ResolveValue(ref, new RowContext(ctx, b, null))
        )
      }

      tbl.sortWith(fn)
    }
    case None => tbl
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
