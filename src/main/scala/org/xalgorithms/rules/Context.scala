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

import org.xalgorithms.rules.elements._
import play.api.libs.json._
import scala.collection.mutable

abstract class Context {
  def load(ptref: PackagedTableReference)
  def retain_map(section: String, m: Map[String, IntrinsicValue])
  def retain_table(section: String, key: String, t: Seq[Map[String, IntrinsicValue]])
  def lookup_in_map(section: String, key: String): Option[IntrinsicValue]
  def lookup_table(section: String, table_name: String): Seq[Map[String, IntrinsicValue]]
  def revisions(): Map[TableReference, Seq[Revision]]
  def revise_table(ref: TableReference, rev: Revision)
  def serialize: JsValue
}

class GlobalContext(load: LoadTableSource) extends Context {
  val _sections = new Sections(Some(load))
  var _revisions = mutable.Map[TableReference, Seq[Revision]]()

  def enumerate_tables(fn: (String, String, Seq[Map[String, IntrinsicValue]]) => Unit): Unit = {
    // _tables.foreach { case (section, tables) =>
    //   tables.foreach { case (name, table) =>
    //     fn(section, name, table)
    //   }
    // }
  }

  def load(ptref: PackagedTableReference) {
    _sections.tables.remember(ptref)
  }

  def retain_map(section: String, m: Map[String, IntrinsicValue]) {
    _sections.retain_values(section, m)
  }

  def retain_table(section: String, key: String, t: Seq[Map[String, IntrinsicValue]]) {
    // section is ignored
    _sections.tables.retain(key, t)
  }

  def lookup_in_map(section: String, key: String): Option[IntrinsicValue] = {
    _sections.values(section) match {
      case Some(vals) => vals.lookup(key)
      case None => None
    }
  }

  def lookup_table(section: String, table_name: String): Seq[Map[String, IntrinsicValue]] = {
    // section is ignored
    _sections.tables.lookup(table_name).getOrElse(null)
  }

  def revisions(): Map[TableReference, Seq[Revision]] = {
    return _revisions.toMap
  }

  def revise_table(ref: TableReference, rev: Revision) {
    _revisions.put(ref, _revisions.getOrElse(ref, mutable.Seq()) :+ rev)
  }

  def serialize: JsValue = {
    Json.obj(
      "documents" -> serialize_maps,
      "tables" -> serialize_tables
    )
  }

  implicit val val_writes = new Writes[IntrinsicValue] {
    def writes(v: IntrinsicValue) = v match {
      case (sv: StringValue) => JsString(sv.value)
      case (nv: NumberValue) => JsNumber(nv.value)
      case _ => {
        println(v)
        JsString("")
      }
    }
  }

  private def serialize_maps: JsValue = {
    JsString("")
    //Json.toJson(_maps)
  }

  private def serialize_tables: JsValue = {
    JsString("")
    // Json.toJson(_tables)
  }
}

class RowContext(
  val ctx: Context,
  val local_row: Map[String, IntrinsicValue],
  val context_row: Map[String, IntrinsicValue]
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

  def revisions(): Map[TableReference, Seq[Revision]] = ctx.revisions()

  def revise_table(ref: TableReference, rev: Revision) = ctx.revise_table(ref, rev)

  def serialize = ctx.serialize
}
