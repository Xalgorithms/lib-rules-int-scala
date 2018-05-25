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
package org.xalgorithms.rules.steps

import org.xalgorithms.rules.{ Context, Change, ChangeOps, Revision }
import org.xalgorithms.rules.elements._

// SYNTAX
// REVISE <rev_table_ref>
//   (ADD|UPDATE) <key> FROM <src_table_ref> WHEN (condition)+)
//   REMOVE key WHEN (condition)+
//
// local   => keyspace of the revised table
// context => keyspace of the source table (ADD, UPDATE) only
//
// OPERATIONS
// - ADD
//   - pulls <key> from <src_table_ref> into EVERY row of <rev_table_ref>
//   - does nothing if a row in <rev_table_ref> contains <key>
// - UPDATE
//   - pulls <key> from <src_table_ref> into EVERY row of
//     <rev_table_ref> ONLY IF <key> exists in the row
//   - does nothing if a row in <rev_table_ref> DOES NOT contain <key>
// - REMOVE
//   - removes <key> from <rev>
//
// CONDICTIONS
// - affect which rows in the revised table are affected
//
//
// COMMENTARY
//
// Conceptually, this step is three stages:
//
// 1. CONCAT: ADD or UPDATE represent a concatination of the revised
//    and source table; it is assumed that the author of the rule has
//    already prepared the tables for REVISE. If <src_table_ref> has a
//    different row count from <rev_table_ref>, then the row count of
//    <rev_table_ref> will be the final size of the table.
// 2. FILTER: The conditions on the modifications filter the rows that
//    can participate in the revision
// 3. REVISE: The revision is recorded according to the change
//    operation
//
// EXAMPLES (represented using a pseudo-code):
//
// table0:
// [
//   { "a" : 0, "b": 2 },
//   { "a" : 1, "b": 4 },
//   { "a" : 2, "b": 6 }
// ]
// table1:
// [
//   { "a" : 0, "c": 3 },
//   { "a" : 1, "c": 6 },
//   { "a" : 2, "c": 9 }
// ]
// table2:
// [
//   { "a" : 2, "c": 3" },
//   { "a" : 3, "c": 6" },
// ]
//
// EXAMPLE0
// 
// REVISE table0 ADD c FROM table1
//
// would yield:
//
// [
//   Add({ "c" : "3" }),
//   Add({ "c" : "6" }),
//   Add({ "c" : "9" })
// ]
//
// Adding a WHEN to this REVISE could refine the result while
// retaining the row count:
//
// REVISE table0 ADD c FROM table1 WHEN @c < 9 WHEN a > 1
//
// [
//   Add({ }),
//   Add({ "c" : "6" }),
//   Add({ })
// ]
//
// would yield:
//
// EXAMPLE1
// 
// REVISE table0 ADD c FROM table2
//
// would yield:
//
// [
//   Add({ "c" : "3" }),
//   Add({ "c" : "6" }),
//   Add({ })
// ]
//
// EXAMPLE2
// 
// REVISE table0 ADD a FROM table2
//
// would yield (due to 'a' existing):
//
// [
//   Add({ }),
//   Add({ }),
//   Add({ })
// ]
//
// EXAMPLE3
// 
// REVISE table0 UPDATE c FROM table2
//
// would yield (due to 'c' NOT existing):
//
// [
//   Update({ }),
//   Update({ }),
//   Update({ })
// ]
//
// EXAMPLE4
// 
// REVISE table0 UPDATE (a, c) FROM table2
//
// would yield (due to 'a' existing):
//
// [
//   Update({ "a" : "2" }),
//   Update({ "a" : "3" }),
//   Update({ })
// ]
//
// Adding a WHEN to this REVISE could refine the result while
// retaining the row count:
//
// REVISE table0 ADD c FROM table1 WHEN a >= 1 WHEN @a < 3
//
// would yield:
//
// [
//   Update({ "a" : "2" }),
//   Update({ }),
//   Update({ })
// ]
//
class ReviseStep(val table: TableReference, val sources: Seq[RevisionSource]) extends Step {
  def execute(ctx: Context) {
    val all_changes = sources.map(_.evaluate(ctx)).foldLeft(
      Seq[Seq[Option[Change]]]()
    ) { (changes, src_changes) =>
      if (changes.size == 0) {
        src_changes.map { ch => Seq(ch) }
      } else {
        (changes, src_changes).zipped.map { case (a, ch) => a :+ ch }
      }
    }

    val table_changes = ctx.lookup_table(table.section, table.name).indices.map { i =>
      if (i < all_changes.size) {
        all_changes(i)
      } else {
        Seq(None)
      }
    }
    ctx.revise_table(table, new Revision(table_changes))
  }
}

