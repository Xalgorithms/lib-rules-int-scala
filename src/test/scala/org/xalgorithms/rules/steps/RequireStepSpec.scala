package org.xalgorithms.rules.steps

import org.scalatest._
import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._
import org.xalgorithms.rules.steps._

class RequireSpec extends FlatSpec with Matchers {
  "RequireStep" should "load tables into the GlobalContext" in {
    val ctx = new GlobalContext(new ResourceLoadTableSource())

    val ref = new PackagedTableReference("package", "table0", "0.0.1", "table0")
    val step = new RequireStep(ref, Seq())

    step.execute(ctx)

    val tbl = ctx.lookup_table("table", "table0")

    tbl should not be null
    tbl.length shouldEqual(3)
    tbl(0)("a") shouldBe a [NumberValue]
    tbl(0)("a").asInstanceOf[NumberValue].value shouldEqual(1.0)
    tbl(0)("b") shouldBe a [StringValue]
    tbl(0)("b").asInstanceOf[StringValue].value shouldEqual("foo")
    tbl(1)("a") shouldBe a [NumberValue]
    tbl(1)("a").asInstanceOf[NumberValue].value shouldEqual(2.0)
    tbl(1)("b") shouldBe a [StringValue]
    tbl(1)("b").asInstanceOf[StringValue].value shouldEqual("bar")
    tbl(2)("a") shouldBe a [NumberValue]
    tbl(2)("a").asInstanceOf[NumberValue].value shouldEqual(3.0)
    tbl(2)("b") shouldBe a [StringValue]
    tbl(2)("b").asInstanceOf[StringValue].value shouldEqual("baz")
  }

  it should "flatten JSON hierarchies when loading" in {
    val ctx = new GlobalContext(new ResourceLoadTableSource())

    val ref = new PackagedTableReference("package", "table0", "0.0.1", "table_hier")
    val step = new RequireStep(ref, Seq())

    step.execute(ctx)

    val tbl = ctx.lookup_table("table", "table_hier")

    tbl should not be null
    tbl.length shouldEqual(2)

    tbl(0).exists(_._1 == "a.aa") shouldBe true
    tbl(0)("a.aa") shouldBe a [StringValue]
    tbl(0)("a.aa").asInstanceOf[StringValue].value shouldEqual("00")
    tbl(0).exists(_._1 == "a.ab") shouldBe true
    tbl(0)("a.ab") shouldBe a [StringValue]
    tbl(0)("a.ab").asInstanceOf[StringValue].value shouldEqual("01")
    tbl(0).exists(_._1 == "b") shouldBe true
    tbl(0)("b") shouldBe a [StringValue]
    tbl(0)("b").asInstanceOf[StringValue].value shouldEqual("02")

    tbl(1).exists(_._1 == "a") shouldBe true
    tbl(1)("a") shouldBe a [StringValue]
    tbl(1)("a").asInstanceOf[StringValue].value shouldEqual("10")
    tbl(1).exists(_._1 == "b.ba") shouldBe true
    tbl(1)("b.ba") shouldBe a [StringValue]
    tbl(1)("b.ba").asInstanceOf[StringValue].value shouldEqual("11")
    tbl(1).exists(_._1 == "b.bb") shouldBe true
    tbl(1)("b.bb") shouldBe a [StringValue]
    tbl(1)("b.bb").asInstanceOf[StringValue].value shouldEqual("12")
  }
}
