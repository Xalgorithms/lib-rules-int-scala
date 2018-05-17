// Copyright 2018 Don Kelly <karfai@gmail.com>

// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License. You may
// obtain a copy of the License at

// http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied. See the License for the specific language governing
// permissions and limitations under the License.

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
