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
import org.xalgorithms.rules.elements._
import play.api.libs.json._
import scala.collection.mutable
import scala.io.Source

class LoadJsonFileTableSource(dn: String) extends LoadJsonTableSource {
  def read(ptref: PackagedTableReference): JsValue = {
    Json.parse((dn / s"${ptref.id}.table.json").contentAsString)
  }
}

class Times {
  val _times = mutable.Map[String, Long]()

  def start(label: String) {
    _times.put(label, System.nanoTime())
  }

  def stop(label: String) {
    val now = System.nanoTime()
    _times.put(label, now - _times.getOrElse(label, now))
  }

  def show() {
    _times.foreach { case (label, t) =>
      println(s"${label}: ${t / 1000000}ms (${t}ns)")
    }
  }
}

object Runner {
  def internalize_object(o: JsObject): Map[String, IntrinsicValue] = {
    o.fields.foldLeft(Map[String, IntrinsicValue]()) { (m, tup) =>
      tup._2 match {
        case (s: JsString)   => m ++ Map(tup._1 -> new StringValue(s.value))
        case (n: JsNumber)   => m ++ Map(tup._1 -> new NumberValue(n.value))
        case (cho: JsObject) => m ++ internalize_object(cho).map { case (k, v) => s"${tup._1}.${k}" -> v }
        case _ => m
      }
    }
  }

  def internalize_array(a: JsArray): Seq[Map[String, IntrinsicValue]] = {
    a.value.foldLeft(Seq[Map[String, IntrinsicValue]]()) { (seq, v) =>
      v match {
        case (o: JsObject) => seq :+ internalize_object(o)
        case _ => seq
      }
    }
  }

  def populate_context(ctx: Context, dir: String): Unit = {
    try {
      Json.parse((dir / "context.json").contentAsString()) match {
        case (o: JsObject) => {
          o.fields.foreach { case (k, v) =>
            v match {
              case (cho: JsObject) => {
                println(s"# adding map to context (k=${k})")
                println(internalize_object(cho))
                ctx.retain_map(k, internalize_object(cho))
              }

              case (cha: JsArray) => {
                println(s"# adding table to context (k=${k})")
                ctx.retain_table("table", k, internalize_array(cha))
              }

              case _ => println(s"? in context, key is neither array nor object (k=${k})")
            }
          }
        }

        case _ => {
        }
      }
    } catch {
      case (e: java.nio.file.NoSuchFileException) => {
        // nothing
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val times = new Times()

    File(args.head).glob("*.rule.json").foreach { fn =>
      println
      println(s"TEST: ${fn}")

      val ts = new LoadJsonFileTableSource(args.head)
      val ctx = new GlobalContext(ts)

      populate_context(ctx, args.head)

      println(s"> loading rule (${fn})")
      times.start("load")
      val steps = SyntaxFromRaw(fn.contentAsString)
      times.stop("load")
      println("< loaded")

      times.start("execute")
      steps.zipWithIndex.foreach { case (step, i) =>
        println(s"> step${i}")
        times.start(s"step${i}")
        step.execute(ctx)
        times.stop(s"step${i}")
        println(s"< step${i}")
      }
      times.stop("execute")

      println
      println("ALL TABLES")
      println

      ctx.enumerate_tables((section: String, name: String, tbl: Seq[Map[String, IntrinsicValue]]) => {
        println(s"${section}:${name}")
        print_table(tbl)
        println
      })

      println
      println("TIMES")
      times.show()
    }
  }

  def print_table(tbl: Seq[Map[String, IntrinsicValue]]): Unit = {
    tbl.zipWithIndex.foreach { case (row, i) =>
      val initial = ("%6s").format(i.toString())
      val vals = row.foldLeft(Seq[String]()) { case (seq, (k, v)) =>
        val vs = ("%14s").format(v match {
          case (sv: StringValue) => sv.value
          case (nv: NumberValue) => nv.value.toString
          case _ => "?"
        })

        seq :+ s"${k}: ${vs}"
      }.mkString(s"${initial} | ", " | ", " |")
      println(vals)
    }
  }
}
