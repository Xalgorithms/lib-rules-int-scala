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
import play.api.libs.json._

abstract class Value {
  def matches(v: Value, op: String): Boolean
}

abstract class IntrinsicValue extends Value {
  def apply_func(args: Seq[Value], func: String): Option[IntrinsicValue]
  def exactly_equals(v: IntrinsicValue): Boolean
}

class NumberValue(val value: BigDecimal) extends IntrinsicValue {
  override def toString = s"number:${value}"

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
    case "subtract" => Some(difference(args))
    case "multiply" => Some(product(args))
    case "divide" => Some(quotient(args))
    case "min" => Some(minimum(args))
    case "max" => Some(maximum(args))
    case _ => None
  }

  def exactly_equals(v: IntrinsicValue): Boolean = v match {
    case (nv: NumberValue) => nv.value == value
    case _ => false
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

  def product(args: Seq[Value]): IntrinsicValue = {
    new NumberValue(args.foldLeft(value) { (product, v) =>
      v match {
        case (nv: NumberValue) => product * nv.value
        case (sv: StringValue) => {
          try {
            product * BigDecimal(sv.value)
          } catch {
            case _: Throwable => product 
          }
        }
        case _ => product 
      }
    })
  }


  def quotient(args: Seq[Value]): IntrinsicValue = {
    new NumberValue(args.foldLeft(value) { (quotient, v) =>
      v match {
        case (nv: NumberValue) => quotient / nv.value
        case (sv: StringValue) => {
          try {
            quotient / BigDecimal(sv.value)
          } catch {
            case _: Throwable => quotient
          }
        }
        case _ => quotient 
      }
    })
  }


  def difference(args: Seq[Value]): IntrinsicValue = {
    new NumberValue(args.foldLeft(value) { (difference, v) =>
      v match {
        case (nv: NumberValue) => difference - nv.value
        case (sv: StringValue) => {
          try {
            difference - BigDecimal(sv.value)
          } catch {
            case _: Throwable => difference 
          }
        }
        case _ => difference 
      }
    })
  }

  def minimum(args: Seq[Value]): IntrinsicValue = {
    new NumberValue(args.foldLeft(value) { (minimum, v) =>
      v match {
        case (nv: NumberValue) => (if (minimum < nv.value) minimum else nv.value) 
        case (sv: StringValue) => {
          try {
            var cv = BigDecimal(sv.value)
            (if ( minimum < cv ) minimum else cv)
          } catch {
            case _: Throwable => minimum 
          }
        }
        case _ => minimum 
      }
    })
  }

  def maximum(args: Seq[Value]): IntrinsicValue = {
    new NumberValue(args.foldLeft(value) { (maximum, v) =>
      v match {
        case (nv: NumberValue) => (if (maximum > nv.value) maximum else nv.value)
        case (sv: StringValue) => {
          try {
            var cv = BigDecimal(sv.value)
            (if ( maximum > cv ) maximum else cv)
          } catch {
            case _: Throwable => maximum 
          }
        }
        case _ => maximum 
      }
    })
  }
}

class StringValue(val value: String) extends IntrinsicValue {
  override def toString = s"string:${value}"

  def matches(v: Value, op: String): Boolean = v match {
    case (sv: StringValue) => matches_value(sv.value, op)
    case (nv: NumberValue) => matches_value(nv.value.toString, op)
    case _ => false
  }

  def apply_func(args: Seq[Value], func: String): Option[IntrinsicValue] = func match {
    case "add" => Some(concat(args))
    case _ => None
  }

  def exactly_equals(v: IntrinsicValue): Boolean = v match {
    case (sv: StringValue) => sv.value == value
    case _ => false
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

class SectionReferenceValue(section: String, key: String) extends ReferenceValue(section, key) {
  def resolve(ctx: Context): Option[IntrinsicValue] = ctx.sections.values(section) match {
    case Some(sec) => sec.lookup(key)
    case None => None
  }

  def update(ctx: Context, v: IntrinsicValue) = ctx.sections.values(section) match {
    case Some(sec) => sec.retain(key, v)
    case None => {}
  }
}

object ResolveValue {
  def apply(v: Value, ctx: Context): Option[IntrinsicValue] = v match {
    case (cv: ComputedValue) => cv.resolve(ctx)
    case (iv: IntrinsicValue) => Some(iv)
    case _ => None
  }
}

object ResolveManyValues {
  def apply(
    vs: Seq[Value],
    ctx: Context
  ): Seq[Option[IntrinsicValue]] = vs.map(ResolveValue(_, ctx))
}

object ResolveMapOfValues {
  def apply(
    movs: Map[String, Value],
    ctx: Context
  ): Map[String, IntrinsicValue] = {
    movs.foldLeft(Map[String, IntrinsicValue]()) { case (m, (k, v)) =>
      ResolveValue(v, ctx) match {
        case Some(iv) => m + (k -> iv)
        case None => m
      }
    }
  }
}

object ResolveSeqMapOfValues {
  def apply(
    smovs: Seq[Map[String, Value]],
    ctx: Context
  ): Seq[Map[String, IntrinsicValue]] = smovs.map(ResolveMapOfValues(_, ctx))
}

object Implicits {
  implicit val val_writes = new Writes[IntrinsicValue] {
    def writes(v: IntrinsicValue) = v match {
      case (sv: StringValue) => JsString(sv.value)
      case (nv: NumberValue) => JsNumber(nv.value)
      case _ => {
        JsString("")
      }
    }
  }
}
