// Copyright (C) 2018 Don Kelly <karfai@gmail.com>
// Copyright (C) 2018 Hayk Pilosyan <hayk.pilos@gmail.com>

// This file is part of Interlibr, a functional component of an
// Internet of Rules (IoR).

// ACKNOWLEDGEMENTS
// Funds: Xalgorithms Foundation
// Collaborators: Don Kelly, Joseph Potvin and Bill Olders.

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
package org.xalgorithms.rules

import org.xalgorithms.rules.elements._

import play.api.libs.json._

// DEBT
// This class should be tested using the ResourceLoadTableSource test
// class. Right now, it's tested in RequireStepSpec which isn't really
// the correct place.
abstract class LoadJsonTableSource extends LoadTableSource {
  def read(ptref: PackagedTableReference): JsValue

  def load(ptref: PackagedTableReference): Seq[Map[String, IntrinsicValue]] = {
    make_table(read(ptref))
  }

  def flatten(v: JsValue): Map[String, IntrinsicValue] = v match {
    case (o: JsObject) => o.fields.foldLeft(Map[String, IntrinsicValue]()) { (m, tup) =>
      m ++ make_value(tup._1, tup._2)
    }
    case _ => Map()
  }

  def make_table(v: JsValue): Seq[Map[String, IntrinsicValue]] = v match {
    case (a: JsArray)  => a.value.map(flatten)
    case _ => Seq()
  }

  def make_value(k: String, v: JsValue): Map[String, IntrinsicValue] = v match {
    case (n: JsNumber) => Map(k -> new NumberValue(n.value))
    case (o: JsObject) => flatten(o).map(tup => (s"${k}.${tup._1}", tup._2))
    case _ => Map(k -> new StringValue(v.validate[String].getOrElse(null)))
  }
}
