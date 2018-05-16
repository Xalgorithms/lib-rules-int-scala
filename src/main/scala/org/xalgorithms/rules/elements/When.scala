package org.xalgorithms.rules.elements

import org.xalgorithms.rules.{ Context }
import org.xalgorithms.rules.elements._

class When(val left: Value, val right: Value, val op: String) {
  def evaluate(ctx: Context): Boolean = {
    ResolveValue(left, ctx) match {
      case Some(lv) => {
        ResolveValue(right, ctx) match {
          case Some(rv) => lv.matches(rv, op)
          case None => false
        }
      }
      case None => false
    }
  }
}

object EvaluateMany {
  def apply(ctx: Context, whens: Iterable[When]): Boolean = {
    whens.foldLeft(true) { (v, wh) => v && wh.evaluate(ctx) }
  }
}
