package org.xalgorithms.rules

import org.bson.{ BsonArray, BsonDocument }
import org.scalatest._
import scala.io.Source

import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._

class LoadJsonTableSourceSpec extends FlatSpec with Matchers {
  class ResourceLoadBsonSource extends LoadBsonTableSource {
    def read(ptref: PackagedTableReference): BsonArray = {
      BsonArray.parse(Source.fromURL(getClass.getResource(s"/${ptref.name}.json")).mkString)
    }
  }

  "LoadJsonTableSourceSpec" should "convert BsonDocuments to internal tables" in {
    val loader = new ResourceLoadBsonSource()

    val tbl0 = loader.load(new PackagedTableReference("table", "table0", "0.0.1", "table0"))

    tbl0.length shouldEqual(3)

    tbl0(0).exists(_._1 == "a") shouldBe true
    tbl0(0)("a") shouldBe a [NumberValue]
    tbl0(0)("a").asInstanceOf[NumberValue].value shouldEqual(1.0)

    tbl0(0).exists(_._1 == "b") shouldBe true
    tbl0(0)("b") shouldBe a [StringValue]
    tbl0(0)("b").asInstanceOf[StringValue].value shouldEqual("foo")

    tbl0(1).exists(_._1 == "a") shouldBe true
    tbl0(1)("a") shouldBe a [NumberValue]
    tbl0(1)("a").asInstanceOf[NumberValue].value shouldEqual(2.0)

    tbl0(1).exists(_._1 == "b") shouldBe true
    tbl0(1)("b") shouldBe a [StringValue]
    tbl0(1)("b").asInstanceOf[StringValue].value shouldEqual("bar")

    tbl0(2).exists(_._1 == "a") shouldBe true
    tbl0(2)("a") shouldBe a [NumberValue]
    tbl0(2)("a").asInstanceOf[NumberValue].value shouldEqual(3.0)

    tbl0(2).exists(_._1 == "b") shouldBe true
    tbl0(2)("b") shouldBe a [StringValue]
    tbl0(2)("b").asInstanceOf[StringValue].value shouldEqual("baz")
  }
}
