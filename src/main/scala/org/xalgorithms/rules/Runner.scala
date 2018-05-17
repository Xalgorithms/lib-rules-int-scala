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
