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
