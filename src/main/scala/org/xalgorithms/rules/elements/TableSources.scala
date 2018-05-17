package org.xalgorithms.rules.elements

class TableSource(val whens: Seq[When]) {
}

class ColumnTableSource(val name: String, val source: String, whens: Seq[When]) extends TableSource(whens) {
}

class ColumnsTableSource(val columns: Seq[String], whens: Seq[When]) extends TableSource(whens) {
}

