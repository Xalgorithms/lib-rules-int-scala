// Copyright (C) 2018-2019 Don Kelly <karfai@gmail.com>
// Copyright (C) 2018-2019 Hayk Pilosyan <hayk.pilos@gmail.com>

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
import scala.collection.mutable

class TableSection(val opt_src: Option[LoadTableSource] = None) {
  val _tables = mutable.Map[String, Seq[Map[String, IntrinsicValue]]]()
  val _refs = mutable.Map[String, PackagedTableReference]()

  def lookup(k: String): Option[Seq[Map[String, IntrinsicValue]]] = {
    _tables.get(k) match {
      case Some(tbl) => Some(tbl)
      case None => maybe_load(k)
    }
  }

  def retain(k: String, tbl: Seq[Map[String, IntrinsicValue]]) = {
    _tables.put(k, tbl)
  }

  def remember(ptref: PackagedTableReference) = {
    _refs.put(ptref.name, ptref)
  }

  private def maybe_load(k: String): Option[Seq[Map[String, IntrinsicValue]]] = _refs.get(k) match {
    case Some(ptref) => {
      opt_src match {
        case Some(src) => {
          retain(ptref.name, opt_src.get.load(ptref))
          _tables.get(k)
        }
        case None => None
      }
    }

    case None => None
  }
}

class ValuesSection(opt_vals: Option[Map[String, IntrinsicValue]] = None) {
  val _values = opt_vals match {
    case Some(vals) => mutable.Map(vals.toSeq: _*)
    case None       => mutable.Map[String, IntrinsicValue]()
  }

  def lookup(k: String): Option[IntrinsicValue] = _values.get(k)
  def retain(k: String, v: IntrinsicValue) = _values.put(k, v)
}

class Sections {
  val _tables = new TableSection()
  val _values = Map[String, ValuesSection]()

  def tables(): TableSection = _tables
  def values(k: String): Option[ValuesSection] = _values.get(k)
}
