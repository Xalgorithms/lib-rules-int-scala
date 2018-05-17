package org.xalgorithms.rules

import org.xalgorithms.rules.elements.{ IntrinsicValue, PackagedTableReference }

import play.api.libs.json._

abstract class LoadTableSource {
  def load(ptref: PackagedTableReference): Seq[Map[String, IntrinsicValue]]
}
