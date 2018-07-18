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
import java.nio.file._
import org.xalgorithms.rules._
import org.xalgorithms.rules.steps._
import org.xalgorithms.rules.elements._
import play.api.libs.json._
import scala.collection.mutable
import scala.io.Source

class LoadJsonFileTableSource(dn: Path) extends LoadJsonTableSource {
  def read(ptref: PackagedTableReference): JsValue = {
    val f = (dn.toString / "tables" / ptref.package_name / ptref.version / s"${ptref.id}.json")
//    println(s"# trying to read table (f=${f}; ptref=${ptref.package_name}:${ptref.id}:${ptref.version})")
    try {
      Json.parse(f.contentAsString)
    } catch {
      case (th: NoSuchFileException) => {
        println(s"! file not found (f=${f})")
        JsNull
      }

      case (th: Throwable) => {
        println(s"unknown error (#{th})")
        JsNull
      }
    }
  }
}

class Times {
  val _times = mutable.Map[String, Long]()
  val _order = mutable.ListBuffer[String]()
  val _labels = mutable.Stack[String]()

  private def keep(t: Long) = {
    val k = top_key
    _times.put(k, t)
    k
  }

  private def top_key = _labels.reverse.mkString(" > ")

  def start(label: String) {
    _labels.push(label)
    _order += keep(System.nanoTime())
  }

  def stop() {
    val now = System.nanoTime()
    keep(now - _times.getOrElse(top_key, now))
    _labels.pop()
  }

  def show(prefix: String = "") {
    _order.foreach { label =>
      val t = _times(label)
      println(s"${prefix}${label}: ${t / 1000000}ms (${t}ns)")
    }
  }
}

object Runner {
  case class TestRun(dir_name: String, run_name: String) {
    private val _times = new Times
    private val _dir = File(dir_name)
    private val _compiled_fn = s"${run_name}.rule.json"
    private val _expect_fn = s"${run_name}.expected.json"
    private val _context_fn = s"${run_name}.context.json"
    private val _ctx = new GlobalContext(new LoadJsonFileTableSource(_dir.path))


    private def load_steps = {
      _times.start("load")
      try {
        Some(SyntaxFromRaw((_dir / _compiled_fn).contentAsString))
      } catch {
        case (th: Throwable) => {
          println(s"! compiled rule does not exist (fn=${_compiled_fn})")
          None
        }
      } finally {
        _times.stop()
      }
    }

    private def load_expected = {
      _times.start("load_expected")
      try {
        Some(Json.parse((_dir / _expect_fn).contentAsString()))
      } catch {
        case (th: Throwable) => {
          None
        }
      } finally {
        _times.stop()
      }
    }

    private def internalize_object(o: JsObject): Map[String, IntrinsicValue] = {
      o.fields.foldLeft(Map[String, IntrinsicValue]()) { (m, tup) =>
        tup._2 match {
          case (s: JsString)   => m ++ Map(tup._1 -> new StringValue(s.value))
          case (n: JsNumber)   => m ++ Map(tup._1 -> new NumberValue(n.value))
          case (cho: JsObject) => m ++ internalize_object(cho).map { case (k, v) => s"${tup._1}.${k}" -> v }
          case _ => m
        }
      }
    }

    private def internalize_array(a: JsArray): Seq[Map[String, IntrinsicValue]] = {
      a.value.foldLeft(Seq[Map[String, IntrinsicValue]]()) { (seq, v) =>
        v match {
          case (o: JsObject) => seq :+ internalize_object(o)
          case _ => seq
        }
      }
    }

    def populate_context = {
      _times.start("populate_context")
      try {
        Json.parse((_dir / _context_fn).contentAsString()) match {
          case (o: JsObject) => {
            o.fields.foreach { case (k, v) =>
              v match {
                case (cho: JsObject) => {
//                  println(s"# adding map to context (k=${k})")
//                  println(internalize_object(cho))
                  _ctx.retain_map(k, internalize_object(cho))
                }

                case (cha: JsArray) => {
//                  println(s"# adding table to context (k=${k})")
                  _ctx.retain_table("table", k, internalize_array(cha))
                }

                case _ => println(s"? in context, key is neither array nor object (k=${k})")
              }
            }
          }

          case _ => {
          }
        }
      } catch {
        case (e: NoSuchFileException) => {
          // nothing
        }
      } finally {
        _times.stop()
      }
    }

    private def execute_all_steps(steps: Seq[Step]) = {
      _times.start("execute")
      steps.zipWithIndex.foreach { case (step, i) =>
        _times.start(s"step${i}")
        step.execute(_ctx)
        _times.stop()
      }
      _times.stop()
    }

    def execute() {
      println(s"# executing ${run_name}")
      load_steps match {
        case Some(steps) => {
          populate_context
          execute_all_steps(steps)
        }

        case None => {
          println("? no steps to execute")
        }
      }
    }

    private def show_table(tbl: Seq[Map[String, IntrinsicValue]]): Unit = {
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

    def show() {
      load_expected match {
        case Some(v) => v match {
          case (o: JsObject) => {
            println("# checking expectations")
          }
          case _ => {
            println("? expectations exists in the wrong format")
          }
        }

        case None => {
          println("# no expectations exists, dumping tables")
          _ctx.enumerate_tables((section: String, name: String, tbl: Seq[Map[String, IntrinsicValue]]) => {
            println(s"${section}:${name}")
            show_table(tbl)
          })
        }
      }

      println
      println("# timing")
      _times.show("## ")
      println
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      val runs = mutable.ListBuffer[TestRun]()
      File(args.head).glob("*.rule").foreach { f =>
        runs += TestRun(args.head, f.nameWithoutExtension)
      }
      println(s"# discovered ${runs.size} test runs in ${args.head}, executing all...")
      runs.foreach { r =>
        r.execute()
        r.show()
      }
    } catch {
      case (th: java.nio.file.NoSuchFileException) => println(s"! test run does not exist (${args.head})")
      case (th: Throwable) => {
        println(s"! error (${th})")
        th.printStackTrace
      }
    }
  }
}
