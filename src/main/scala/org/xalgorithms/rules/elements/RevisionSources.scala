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

package org.xalgorithms.rules.elements

class RevisionSource(val column: String, val whens: Seq[When]) {
}

class TableRevisionSource(
  column: String, whens: Seq[When], val table: TableReference) extends RevisionSource(column, whens) {
}

class AddRevisionSource(
  column: String, whens: Seq[When], table: TableReference) extends TableRevisionSource(column, whens, table) {
}

class UpdateRevisionSource(
  column: String, whens: Seq[When], table: TableReference) extends TableRevisionSource(column, whens, table) {
}

class DeleteRevisionSource(column: String, whens: Seq[When]) extends RevisionSource(column, whens) {
}
