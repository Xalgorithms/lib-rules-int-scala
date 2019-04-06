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
  def revisions(): Map[TableReference, Seq[Revision]]
  def revise_table(ref: TableReference, rev: Revision)
  def serialize: JsValue

  def sections: Sections
}

class GlobalContext(load: LoadTableSource) extends Context {
  val _sections = new Sections(Some(load))
  val _revisions = mutable.Map[TableReference, Seq[Revision]]()

  def sections = _sections

  def enumerate_tables(
    fn: (String, String, Seq[Map[String, IntrinsicValue]]) => Unit
  ) = _sections.enumerate_tables(fn)

  def revisions(): Map[TableReference, Seq[Revision]] = {
    return _revisions.toMap
  }

  def revise_table(ref: TableReference, rev: Revision) {
    _revisions.put(ref, _revisions.getOrElse(ref, mutable.Seq()) :+ rev)
  }

  def serialize = _sections.serialize

  implicit val val_writes = new Writes[IntrinsicValue] {
    def writes(v: IntrinsicValue) = v match {
      case (sv: StringValue) => JsString(sv.value)
      case (nv: NumberValue) => JsNumber(nv.value)
      case _ => {
        JsString("")
      }
    }
  }
}

class RowContext(
  val ctx: Context,
  val local_row: Map[String, IntrinsicValue],
  val context_row: Map[String, IntrinsicValue]
) extends Context {
  if (local_row != null) {
    ctx.sections.retain_values("_local", local_row)
  }

  if (context_row != null) {
    ctx.sections.retain_values("_context", context_row)
  }

  def sections = ctx.sections

  def revisions(): Map[TableReference, Seq[Revision]] = ctx.revisions()

  def revise_table(ref: TableReference, rev: Revision) = ctx.revise_table(ref, rev)

  def serialize = ctx.serialize
}
