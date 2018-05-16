package org.xalgorithms.rules.elements

import org.scalamock.scalatest.MockFactory
import org.scalatest._

import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._

class ReferencesSpec extends FlatSpec with Matchers with MockFactory {
  def map_to_expected(m: Map[String, String]): Map[String, IntrinsicValue] = {
    m.map { case (k, v) => (k, new StringValue(v)) }
  }

  "TableReference" should "load tables from the Context" in {
    val tables = Map(
      "map0" -> Seq(
        Map("a" -> "00", "b" -> "01"),
        Map("a" -> "10", "b" -> "11")),
      "map1" -> Seq(
        Map("A" -> "xx", "B" -> "yy"),
        Map("A" -> "yy", "B" -> "zz")))
    val ctx = mock[Context]

    tables.foreach { case (key, ex) =>
      val expected = ex.map(map_to_expected)
      val ref = new TableReference("tables", key)

      (ctx.lookup_table _).expects("tables", key).returning(expected)
      ref.get(ctx) shouldEqual(expected)
    }
  }
}
