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

package org.xalgorithms.rules.steps

import org.xalgorithms.rules.{ Context, RowContext }
import org.xalgorithms.rules.elements._

class AssembleStep(val name: String, val columns: Seq[Column]) extends Step {
  def execute(ctx: Context) {
    val combined_tbl = columns.foldLeft(Seq[Map[String, IntrinsicValue]]()) { (tbl, col) =>
      col.sources.size match {
        case 0 => tbl
        case _ => combine(tbl, build_table_from_sources(ctx, col.table, col.sources))
      }
    }

    ctx.retain_table("table", name, combined_tbl)
  }

  def combine(
    left: Seq[Map[String, IntrinsicValue]],
    right: Seq[Map[String, IntrinsicValue]]
  ): Seq[Map[String, IntrinsicValue]] = left.size match {
    case 0 => right
    case _ => left.map { r0 => right.map { r1 => r0 ++ r1 } }.flatten
  }

  def merge(
    left: Seq[Map[String, IntrinsicValue]],
    right: Seq[Map[String, IntrinsicValue]]
  ): Seq[Map[String, IntrinsicValue]] = left.size match {
    case 0 => right
    case _ => (left, right).zipped.map { (rl, rr) => rl ++ rr }
  }

  def build_tuple_from_source(
    tbl: Seq[Map[String, IntrinsicValue]], src: TableSource
  ): Seq[Tuple2[Map[String, IntrinsicValue], Map[String, IntrinsicValue]]] = src match {
    case (cols: ColumnsTableSource) => tbl.map { r =>
      Tuple2(r, r.filterKeys { k => cols.columns.length == 0 || cols.columns.contains(k) })
    }
    case (col: ColumnTableSource) => tbl.map { r =>
      Tuple2(r, Map(col.name -> r(col.source)))
    }
    case _ => tbl.map { r => Tuple2(r, r) }
  }

  def apply_whens(
    ctx: Context,
    tups: Seq[Tuple2[Map[String, IntrinsicValue], Map[String, IntrinsicValue]]],
    whens: Seq[When]
  ): Seq[Map[String, IntrinsicValue]] = {
    tups.foldLeft(Seq[Map[String, IntrinsicValue]]()) { (a, tup) =>
      // original row (_1) is the "context" and new row (_2) is the "local" is
      // this application of a WHEN
      if (EvaluateMany(new RowContext(ctx, tup._2, tup._1), whens)) {
        a :+ tup._2
      } else {
        a
      }
    }
  }

  def build_table_from_sources(
    ctx: Context, tr: TableReference, srcs: Seq[TableSource]
  ): Seq[Map[String, IntrinsicValue]] = {
    val src_tbl = ctx.lookup_table(tr.section, tr.name)
    srcs.foldLeft(Seq[Map[String, IntrinsicValue]]()) { (tbl, src) =>
      // sources are within the SAME table and are the same size, so we zip/merge them together
      // as if this were an append
      val ntbl = apply_whens(ctx, build_tuple_from_source(src_tbl, src), src.whens)

      merge(tbl, ntbl)
    }
  }
}
