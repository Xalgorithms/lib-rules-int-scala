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

import better.files._
import scala.io.Source
import org.xalgorithms.rules.elements._
import play.api.libs.json._

class LoadJsonFileTableSource(dn: String) extends LoadJsonTableSource {
  def read(ptref: PackagedTableReference): JsValue = {
    Json.parse((dn / s"${ptref.id}.table.json").contentAsString)
  }
}

object Runner {
  def main(args: Array[String]): Unit = {
    File(args.head).glob("*.rule.json").foreach { fn =>
      println
      println(s"TEST: ${fn}")

      val ts = new LoadJsonFileTableSource(args.head)
      val ctx = new GlobalContext(ts)
      SyntaxFromRaw(fn.contentAsString).foreach(_.execute(ctx))

      println
      println("ALL TABLES")
      println

      ctx.enumerate_tables((section: String, name: String, tbl: Seq[Map[String, IntrinsicValue]]) => {
        println(s"${section}:${name}")
        print_table(tbl)
        println
      })
    }
  }

  def print_table(tbl: Seq[Map[String, IntrinsicValue]]): Unit = {
    tbl.foreach { row =>
      val vals = row.foldLeft(Seq[String]()) { case (seq, (k, v)) =>
        val vs = ("%10s").format(v match {
          case (sv: StringValue) => sv.value
          case (nv: NumberValue) => nv.value.toString
          case _ => "?"
        })

        seq :+ s"${k}: ${vs}"
      }.mkString("| ", " | ", " |")
      println(vals)
    }
  }
}
