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

class PackagedTableReference(val package_name: String, val id: String, val version: String, val name: String) {
}

class Column(val table: TableReference, val sources: Seq[TableSource]) {
}

class Assignment(val target: String, val source: Value) {
}

