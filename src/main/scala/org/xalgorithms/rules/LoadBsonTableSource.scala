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
package org.xalgorithms.rules

import collection.JavaConverters._
import org.bson._

import org.xalgorithms.rules.elements.{ IntrinsicValue, NumberValue, PackagedTableReference, StringValue }

abstract class LoadBsonTableSource extends LoadTableSource {
  def read(ptref: PackagedTableReference): BsonArray

  def load(ptref: PackagedTableReference): Seq[Map[String, IntrinsicValue]] = {
    make_table(read(ptref))
  }

  def make_table(doc: BsonArray): Seq[Map[String, IntrinsicValue]] = {
    doc.getValues.asScala.map { bv =>
      make_row(bv)
    }
  }

  def make_row(bv: BsonValue): Map[String, IntrinsicValue] = bv match {
    case (d: BsonDocument) => d.keySet.asScala.foldLeft(Map[String, IntrinsicValue]()) { (m, k) =>
      m ++ make_value(k, d.get(k))
    }
    case _ => Map[String, IntrinsicValue]()
  }

  def make_value(k: String, v: BsonValue): Map[String, IntrinsicValue] = v match {
    case (nv: BsonDouble)  => Map(k -> new NumberValue(nv.getValue()))
    case (nv: BsonInt32)  => Map(k -> new NumberValue(nv.doubleValue()))
    case (nv: BsonInt64)  => Map(k -> new NumberValue(nv.doubleValue()))
    case _ => Map(k -> new StringValue(v.asString.getValue()))
  }
}
