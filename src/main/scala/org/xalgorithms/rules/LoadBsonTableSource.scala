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
