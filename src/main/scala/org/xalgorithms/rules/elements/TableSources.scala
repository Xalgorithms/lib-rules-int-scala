// Copyright (C) 2018 Don Kelly <karfai@gmail.com>
// Copyright (C) 2018 Hayk Pilosyan <hayk.pilos@gmail.com>

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
package org.xalgorithms.rules.elements

class TableSource(val whens: Seq[When]) {
}

class ColumnTableSource(val name: String, val source: String, whens: Seq[When]) extends TableSource(whens) {
}

class ColumnsTableSource(val columns: Seq[String], whens: Seq[When]) extends TableSource(whens) {
}

