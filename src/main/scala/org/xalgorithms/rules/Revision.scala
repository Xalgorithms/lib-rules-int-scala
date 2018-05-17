package org.xalgorithms.rules

import org.xalgorithms.rules.elements.{ Value }

object ChangeOps extends Enumeration {
  val Add, Remove, Update = Value
}

class Change(val op: ChangeOps.Value, val value: Value) {
}

class Revision(changes: Seq[Map[String, Change]]) {
  var _changes: scala.collection.mutable.Seq[Map[String, Change]] =
    scala.collection.mutable.Seq() ++ changes

  def changes(): Seq[Map[String, Change]] = {
    return _changes.toSeq
  }
}
