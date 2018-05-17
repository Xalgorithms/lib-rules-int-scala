package org.xalgorithms.rules.elements

import org.xalgorithms.rules.{ Context }

class TableReference(val section: String, val name: String) {
  def get(ctx: Context): Seq[Map[String, Value]] = {
    ctx.lookup_table(section, name)
  }
}
