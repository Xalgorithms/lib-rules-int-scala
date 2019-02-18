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

class LoadJsonFileTableSource(dn: Path, tables_index: Map[String, Option[String]], times: Times) extends LoadJsonTableSource {
  def read_json(fn: String): JsValue = {
    times.start(s"load_json/${fn}")
    try {
      Json.parse((dn.toString / fn).contentAsString)
    } catch {
      case (th: NoSuchFileException) => {
        println(s"! file not found (fn=${fn})")
        JsNull
      }

      case (th: Throwable) => {
        println(s"unknown error (${th})")
        JsNull
      }
    } finally {
      times.stop
    }
  }

  def read(ptref: PackagedTableReference): JsValue = {
    tables_index.get(s"${ptref.id}:${ptref.version}").flatMap { opt_data_fn =>
      opt_data_fn.map(read_json(_))
    } match {
      case None => JsNull
      case Some(v) => v
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
  case class TestRun(dir_name: String, run_name: String, tables_index: Map[String, Option[String]]) {
    private val _times = new Times
    private val _dir = File(dir_name)
    private val _context_fn = s"${run_name}.context.json"

    private def warn(s: String) = Console.YELLOW + s + Console.RESET
    private def error(s: String) = Console.RED + s + Console.RESET
    private def ok(s: String) = Console.GREEN + s + Console.RESET
    private def title(s: String) = Console.BOLD + s.toUpperCase + Console.RESET
    private def subtitle(s: String) = Console.UNDERLINED + s + Console.RESET

    private def load_steps = {
      _dir.glob("*.rule").foldLeft(Map[String, Seq[Step]]()) { (m, f) =>
        println(s"> loading ${f.name}")
        val rule_name = f.name.stripSuffix(".rule")
        val compiled_fn = s"${rule_name}.rule.json"
        _times.start(s"load/${rule_name}")
        try {
          m + (rule_name -> SyntaxFromRaw((_dir / compiled_fn).contentAsString))
        } catch {
          case (th: Throwable) => {
            println(s"! compiled rule does not exist (fn=${compiled_fn})")
            m
          }
        } finally {
          _times.stop()
        }
      }
    }

    private def load_expected(rule_name: String) = {
      val expect_fn = s"${rule_name}.expected.json"
      _times.start(s"load/expected/${expect_fn}")
      try {
        Some(Json.parse((_dir / expect_fn).contentAsString()))
      } catch {
        case (th: Throwable) => {
          println(th)
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
      val ctx = new GlobalContext(new LoadJsonFileTableSource(_dir.path, tables_index, _times))
      try {
        Json.parse((_dir / _context_fn).contentAsString()) match {
          case (o: JsObject) => {
            o.fields.foreach { case (k, v) =>
              v match {
                case (cho: JsObject) => {
//                  println(s"# adding map to context (k=${k})")
//                  println(internalize_object(cho))
                  ctx.retain_map(k, internalize_object(cho))
                }

                case (cha: JsArray) => {
//                  println(s"# adding table to context (k=${k})")
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
        case (e: NoSuchFileException) => {
          // nothing
        }
      } finally {
        _times.stop()
      }

      ctx
    }

    private def execute_all_steps(ctx: Context, steps: Seq[Step]) = {
      _times.start("execute")
      steps.zipWithIndex.foreach { case (step, i) =>
        _times.start(s"step${i}")
        step.execute(ctx)
        _times.stop()
      }
      _times.stop()
    }

    def execute() {
      println(title(s"execute: ${run_name}"))
      _times.start("load")
      val steps = load_steps
      _times.stop

      steps.foreach { case (rule_name, steps) =>
        println(subtitle(s"> evaluate: ${rule_name} (${steps.length} steps)"))
        val ctx = populate_context
        _times.start(s"execute/${rule_name}")
        execute_all_steps(ctx, steps)
        _times.stop

        show(ctx, rule_name)
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

    def find_mismatches(ex: Map[String, IntrinsicValue], ac: Map[String, IntrinsicValue]) = {
      ex.foldLeft(Seq[String]()) { (seq, kv) =>
        ac.get(kv._1) match {
          case Some(v) => {
            if (kv._2.exactly_equals(v)) {
              seq
            } else {
              seq :+ s"(${kv._1}): expected ${kv._2}, got ${v}"
            }
          }

          case None => seq :+ s"(${kv._1}): missing value"
        }
      }
    }

    def compare_and_show(
      section: String,
      name: String,
      ex_tbl: Seq[Map[String, IntrinsicValue]],
      ac_tbl: Seq[Map[String, IntrinsicValue]]
    ) {
      if (ex_tbl.size == ac_tbl.size) {
        val diffs = ex_tbl.zip(ac_tbl).zipWithIndex.foldLeft(Seq[(Int, Seq[String])]()) { (seq, tup) =>
          val mismatches = find_mismatches(tup._1._1, tup._1._2)
          if (mismatches.size > 0) {
            seq :+ (tup._2, mismatches)
          } else {
            seq
          }
        }
        if (diffs.size > 0) {
          println(error(s"! ${section}:${name} => FAIL"))
          diffs.foreach { case (ri, problems) =>
            problems.foreach { problem =>
              println(warn(s"  [${ri}]: ${problem}"))
            }
          }
        } else {
          println(ok(s"> ${section}:${name} => OK"))
        }
      } else {
        println(error(s"! ${section}:${name} => FAIL"))
        println(warn(s"  tables are different sizes (ex=${ex_tbl.size}; ac=${ac_tbl.size})"))
      }
    }

    def show(ctx: GlobalContext, rule_name: String) {
      load_expected(rule_name) match {
        case Some(v) => v match {
          case (o: JsObject) => {
            println(subtitle("expectations"))
            ctx.enumerate_tables((section: String, name: String, tbl: Seq[Map[String, IntrinsicValue]]) => {
              (o \ "tables" \ section \ name).asOpt[JsArray].map(internalize_array(_)) match {
                case Some(ex_tbl) => {
                  compare_and_show(section, name, ex_tbl, tbl)
                }
                case None => {}
              }
            })
          }
          case _ => {
            println("? expectations exist in the wrong format")
          }
        }

        case None => {
          println(warn("# no expectations exist, dumping tables"))
          ctx.enumerate_tables((section: String, name: String, tbl: Seq[Map[String, IntrinsicValue]]) => {
            println(s"${section}:${name}")
            show_table(tbl)
          })
        }
      }

      println
      println(subtitle("timing"))
      _times.show("> ")
      println
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      val runs = mutable.ListBuffer[TestRun]()
      val tables = File(args.head).glob("*.table.json").foldLeft(Map[String, Option[String]]()) { (m, f) =>
        val jv = Json.parse(f.contentAsString)
        jv match {
          case (o: JsObject) => {
            val opt_version = (o \ "meta" \ "version").asOpt[String]
            val opt_data_fn = (o \ "data").asOpt[Seq[JsValue]].flatMap { seq =>
              (seq.head \ "location").asOpt[String]
            }

            val k = (Some(f.nameWithoutExtension) ++ opt_version).mkString(":")
            m ++ Map(k -> opt_data_fn)
          }

          case _ => m
        }
      }

      File(args.head).glob("*.rule").foreach { f =>
        runs += TestRun(args.head, f.nameWithoutExtension, tables)
      }
      println(s"# discovered ${runs.size} test runs in ${args.head}, executing all...")
      runs.foreach { r => r.execute() }
    } catch {
      case (th: java.nio.file.NoSuchFileException) => println(s"! test run does not exist (${args.head})")
      case (th: Throwable) => {
        println(s"! error (${th})")
        th.printStackTrace
      }
    }
  }
}
