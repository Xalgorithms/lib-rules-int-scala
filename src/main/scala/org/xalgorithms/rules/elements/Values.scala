// Copyright (C) 2018 Don Kelly <karfai@gmail.com>
// Copyright (C) 2018 Hayk Pilosyan <hayk.pilos@gmail.com>

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
package org.xalgorithms.rules.elements

import org.xalgorithms.rules.{ Context }

abstract class Value {
  def matches(v: Value, op: String): Boolean
}

abstract class IntrinsicValue extends Value {
  def apply_func(args: Seq[Value], func: String): Option[IntrinsicValue]
}

class NumberValue(val value: BigDecimal) extends IntrinsicValue {
  def matches(v: Value, op: String): Boolean = v match {
    case (sv: StringValue) => matches_value(BigDecimal(sv.value), op)
    case (nv: NumberValue) => matches_value(nv.value, op)
    case _ => false
  }

  def matches_value(v: BigDecimal, op: String): Boolean = op match {
    case "eq" => value == v
    case "lt" => value < v
    case "lte" => value <= v
    case "gt" => value > v
    case "gte" => value >= v
    case _ => false
  }

  def apply_func(args: Seq[Value], func: String): Option[IntrinsicValue] = func match {
    case "add" => Some(sum(args))
    case _ => None
  }

  def sum(args: Seq[Value]): IntrinsicValue = {
    new NumberValue(args.foldLeft(value) { (sum, v) =>
      v match {
        case (nv: NumberValue) => sum + nv.value
        case (sv: StringValue) => {
          try {
            sum + BigDecimal(sv.value)
          } catch {
            case _: Throwable => sum
          }
        }
        case _ => sum
      }
    })
  }
}

class StringValue(val value: String) extends IntrinsicValue {
  def matches(v: Value, op: String): Boolean = v match {
    case (sv: StringValue) => matches_value(sv.value, op)
    case (nv: NumberValue) => matches_value(nv.value.toString, op)
    case _ => false
  }

  def apply_func(args: Seq[Value], func: String): Option[IntrinsicValue] = func match {
    case "add" => Some(concat(args))
    case _ => None
  }

  def matches_value(v: String, op: String): Boolean = op match {
    case "eq" => value == v
    case "lt" => value < v
    case "lte" => value <= v
    case "gt" => value > v
    case "gte" => value >= v
    case _ => false
  }

  def concat(args: Seq[Value]): IntrinsicValue = {
    new StringValue(args.foldLeft(value) { (s, v) =>
      v match {
        case (sv: StringValue) => s + sv.value
        case (nv: NumberValue) => s + nv.value.toString
        case _ => s
      }
    })
  }
}

abstract class ComputedValue extends Value {
  def resolve(ctx: Context): Option[IntrinsicValue]
}

class FunctionValue(val name: String, val args: Seq[Value]) extends ComputedValue {
  def matches(v: Value, op: String): Boolean = false

  def resolve(ctx: Context): Option[IntrinsicValue] = {
    val largs = args.foldLeft(Seq[IntrinsicValue]()) { (a, v) =>
      ResolveValue(v, ctx) match {
        case Some(iv) => a :+ iv
        case None => a
      }
    }

    largs.length match {
      case 0 => None
      case 1 => Some(largs.head)
      case _ => largs.head.apply_func(largs.tail, name)
    }
  }
}

abstract class ReferenceValue(val section: String, val key: String) extends ComputedValue {
  def resolve(ctx: Context): Option[IntrinsicValue]

  def matches(v: Value, op: String): Boolean = v match {
    case (vr: ReferenceValue) => if ("eq" == op) section == vr.section && key == vr.key else false
    case _ => false
  }
}

class DocumentReferenceValue(section: String, key: String) extends ReferenceValue(section, key) {
  def resolve(ctx: Context): Option[IntrinsicValue] = {
    ctx.lookup_in_map(section, key)
  }
}

object ResolveValue {
  def apply(v: Value, ctx: Context): Option[IntrinsicValue] = v match {
    case (cv: ComputedValue) => cv.resolve(ctx)
    case (iv: IntrinsicValue) => Some(iv)
    case _ => None
  }
}
