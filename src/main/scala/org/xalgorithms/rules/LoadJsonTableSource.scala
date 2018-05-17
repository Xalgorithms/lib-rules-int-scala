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
