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
import org.xalgorithms.rules.elements._

class When(val left: Value, val right: Value, val op: String) {
  def evaluate(ctx: Context): Boolean = {
    ResolveValue(left, ctx) match {
      case Some(lv) => {
        ResolveValue(right, ctx) match {
          case Some(rv) => lv.matches(rv, op)
          case None => false
        }
      }
      case None => false
    }
  }
}

object EvaluateMany {
  def apply(ctx: Context, whens: Iterable[When]): Boolean = {
    whens.foldLeft(true) { (v, wh) => v && wh.evaluate(ctx) }
  }
}
