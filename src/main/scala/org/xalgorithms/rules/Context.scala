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

import org.xalgorithms.rules.elements.{ PackagedTableReference, IntrinsicValue }
import scala.collection.mutable

abstract class Context {
  def load(ptref: PackagedTableReference)
  def retain_map(section: String, m: Map[String, IntrinsicValue])
  def retain_table(section: String, key: String, t: Seq[Map[String, IntrinsicValue]])
  def lookup_in_map(section: String, key: String): Option[IntrinsicValue]
  def lookup_table(section: String, table_name: String): Seq[Map[String, IntrinsicValue]]
  def revisions(): Map[String, Seq[Revision]]
  def add_revision(key: String, rev: Revision)
}

class GlobalContext(load: LoadTableSource) extends Context {
  var _tables = mutable.Map[String, mutable.Map[String, Seq[Map[String, IntrinsicValue]]]]()
  var _revisions = mutable.Map[String, mutable.Seq[Revision]]()
  var _maps = mutable.Map[String, Map[String, IntrinsicValue]]()

  def enumerate_tables(fn: (String, String, Seq[Map[String, IntrinsicValue]]) => Unit): Unit = {
    _tables.foreach { case (section, tables) =>
      tables.foreach { case (name, table) =>
        fn(section, name, table)
      }
    }
  }

  def load(ptref: PackagedTableReference) {
    retain_table("table", ptref.name, load.load(ptref))
  }

  def retain_map(section: String, m: Map[String, IntrinsicValue]) {
    _maps(section) = m
  }

  def retain_table(section: String, key: String, t: Seq[Map[String, IntrinsicValue]]) {
    val sm = _tables.getOrElse(section, mutable.Map[String, Seq[Map[String, IntrinsicValue]]]())
    sm.put(key, t)
    _tables(section) = sm
  }

  def lookup_in_map(section: String, key: String): Option[IntrinsicValue] = {
    _maps.getOrElse(section, Map[String, IntrinsicValue]()).get(key)
  }

  def lookup_table(section: String, table_name: String): Seq[Map[String, IntrinsicValue]] = {
    _tables.getOrElse(section, mutable.Map[String, Seq[Map[String, IntrinsicValue]]]()).getOrElse(table_name, null)
  }

  def revisions(): Map[String, Seq[Revision]] = {
    return _revisions.toMap
  }

  def add_revision(key: String, rev: Revision) {
    val current = _revisions.getOrElse(key, scala.collection.mutable.Seq())
    _revisions.put(key, current ++ scala.collection.mutable.Seq(rev))
  }
}

class RowContext(
  ctx: Context,
  local_row: Map[String, IntrinsicValue],
  context_row: Map[String, IntrinsicValue]
) extends Context {
  val _local = mutable.Map[String, IntrinsicValue]() ++ local_row


  def load(ptref: PackagedTableReference) = ctx.load(ptref)
  def retain_map(section: String, m: Map[String, IntrinsicValue]) = ctx.retain_map(section, m)
  def retain_table(section: String, key: String, t: Seq[Map[String, IntrinsicValue]]) = ctx.retain_table(section, key, t)

  def update_local(ch: Map[String, IntrinsicValue]): Unit = {
    _local ++= ch
  }

  def lookup_in_map(section: String, key: String): Option[IntrinsicValue] = section match {
    case "_local" => _local.get(key)
    case "_context" => context_row.get(key)
    case _ => ctx.lookup_in_map(section, key)
  }

  def lookup_table(section: String, table_name: String): Seq[Map[String, IntrinsicValue]] = ctx.lookup_table(section, table_name)

  def revisions(): Map[String, Seq[Revision]] = ctx.revisions()

  def add_revision(key: String, rev: Revision) = ctx.add_revision(key, rev)
}
