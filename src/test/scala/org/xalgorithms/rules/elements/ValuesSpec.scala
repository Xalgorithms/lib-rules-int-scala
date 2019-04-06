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
package org.xalgorithms.rules.elements

import org.scalamock.scalatest.MockFactory
import org.scalatest._

import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._

class ValuesSpec extends FlatSpec with Matchers with MockFactory with AppendedClues {
  "NumberValue" should "match or convert" in {
    Seq(
      Tuple4(1.0, new NumberValue(1.0), "eq", true),
      Tuple4(1.0, new NumberValue(2.0), "lt", true),
      Tuple4(2.0, new NumberValue(2.0), "lt", false),
      Tuple4(1.0, new NumberValue(2.0), "lte", true),
      Tuple4(2.0, new NumberValue(2.0), "lte", true),
      Tuple4(3.0, new NumberValue(2.0), "gte", true),
      Tuple4(2.0, new NumberValue(2.0), "gte", true),
      Tuple4(3.0, new NumberValue(2.0), "gt", true),
      Tuple4(2.0, new NumberValue(2.0), "gt", false),
      Tuple4(1.0, new StringValue("1.0"), "eq", true),
      Tuple4(1.0, new StringValue("2.0"), "lt", true),
      Tuple4(2.0, new StringValue("2.0"), "lt", false),
      Tuple4(1.0, new StringValue("2.0"), "lte", true),
      Tuple4(2.0, new StringValue("2.0"), "lte", true),
      Tuple4(3.0, new StringValue("2.0"), "gte", true),
      Tuple4(2.0, new StringValue("2.0"), "gte", true),
      Tuple4(3.0, new StringValue("2.0"), "gt", true),
      Tuple4(2.0, new StringValue("2.0"), "gt", false)
    ).foreach { case (n, rhs, op, ex) =>
        new NumberValue(n).matches(rhs, op) shouldEqual(ex)
    }
  }

  it should "have a precise equivalence based on type" in {
    Seq(
      (1.0, new NumberValue(1.0), true, "number:1.0"),
      (1.0, new NumberValue(2.0), false, "number:2.0"),
      (2.0, new NumberValue(2.0), true, "number:2.0"),
      (1.0, new StringValue("1.0"), false, "string:1.0"),
      (1.0, new StringValue("2.0"), false, "string:2.0"),
      (2.0, new StringValue("2.0"), false, "string:2.0"),
    ).foreach { case (n, rhs, ex, label) =>
        val clue = if (ex) {
          s"expected ${n} to exactly match ${label}"
        } else {
          s"expected ${n} to not exactly match ${label}"
        }
        new NumberValue(n).exactly_equals(rhs) shouldEqual(ex) withClue(clue)
    }
  }

  it should "have a string representation" in {
    new NumberValue(1.0).toString shouldEqual("number:1.0")
    new NumberValue(1.11).toString shouldEqual("number:1.11")
    new NumberValue(54321.12345).toString shouldEqual("number:54321.12345")
  }

  it should "perform addition of basic Values as SUM" in {
    Seq(
      Tuple3(1.0, Seq(new NumberValue(2.2), new NumberValue(3.3), new NumberValue(4.4)), 10.9),
      Tuple3(1.0, Seq(new NumberValue(-2.0), new NumberValue(3.0)), 2.0),
      Tuple3(11.0, Seq(new NumberValue(1.0), new NumberValue(2.0)), 14.0),
      Tuple3(11.0, Seq(new StringValue("1.0"), new NumberValue(2.0)), 14.0),
      Tuple3(11.0, Seq(new StringValue("1.0"), new StringValue("abc"), new StringValue("2.0")), 14.0)
    ).foreach { case (bv, args, ex) =>
        val v = new NumberValue(bv)
        val sum = v.apply_func(args, "add")

        sum match {
          case Some(v) =>
            v shouldBe a [NumberValue]
            v.asInstanceOf[NumberValue].value shouldEqual(ex)
          case None =>
            true shouldEqual(false)
        }
    }
  }

  "StringValue" should "match or convert" in {
    Seq(
      Tuple4("bb", new StringValue("bb"), "eq", true),
      Tuple4("bb", new StringValue("bb"), "lte", true),
      Tuple4("bb", new StringValue("bb"), "gte", true),
      Tuple4("bb", new StringValue("bb"), "lt", false),
      Tuple4("bb", new StringValue("bb"), "gt", false),
      Tuple4("aa", new StringValue("bb"), "lt", true),
      Tuple4("cc", new StringValue("bb"), "gt", true),
      Tuple4("aa", new StringValue("bb"), "gt", false),
      Tuple4("cc", new StringValue("bb"), "lt", false),
      Tuple4("1.0", new NumberValue(1.0), "eq", true),
      Tuple4("1.0", new NumberValue(2.0), "lt", true),
      Tuple4("2.0", new NumberValue(2.0), "lt", false),
      Tuple4("1.0", new NumberValue(2.0), "lte", true),
      Tuple4("2.0", new NumberValue(2.0), "lte", true),
      Tuple4("3.0", new NumberValue(2.0), "gte", true),
      Tuple4("2.0", new NumberValue(2.0), "gte", true),
      Tuple4("3.0", new NumberValue(2.0), "gt", true),
      Tuple4("2.0", new NumberValue(2.0), "gt", false)
    ).foreach { case (n, rhs, op, ex) =>
        new StringValue(n).matches(rhs, op) shouldEqual(ex)
    }
  }

  it should "have a precise equivalence based on type" in {
    Seq(
      ("bb", new StringValue("bb"), true, "string:bb"),
      ("bb", new StringValue("aa"), false, "string:aa"),
      ("1.0", new NumberValue(1.0), false, "number:1.0"),
      ("1.0", new NumberValue(2.0), false, "number:2.0")
    ).foreach { case (n, rhs, ex, label) =>
        val clue = if (ex) {
          s"expected ${n} to exactly match ${label}"
        } else {
          s"expected ${n} to not exactly match ${label}"
        }
        new StringValue(n).exactly_equals(rhs) shouldEqual(ex) withClue(clue)
    }
  }

  it should "have a string representation" in {
    new StringValue("aa").toString shouldEqual("string:aa")
    new StringValue("bb").toString shouldEqual("string:bb")
  }

  it should "perform addition of basic Values as CONCAT" in {
    Seq(
      Tuple3("a", Seq(new StringValue("b"), new StringValue("c")), "abc"),
      Tuple3("a", Seq(new StringValue("1.0"), new NumberValue(2.0)), "a1.02.0"),
      Tuple3("a", Seq(new StringValue("1.0"), new StringValue("abc"), new NumberValue(2.0)), "a1.0abc2.0")
    ).foreach { case (bv, args, ex) =>
        val v = new StringValue(bv)
        val sum = v.apply_func(args, "add")

        sum match {
          case Some(v) =>
            v shouldBe a [StringValue]
            v.asInstanceOf[StringValue].value shouldEqual(ex)
          case None =>
            true shouldEqual(false)
        }
    }
  }

  def match_nothing(mv: Value) {
    Seq(
      Tuple2(new StringValue("bb"), "eq"),
      Tuple2(new StringValue("bb"), "lte"),
      Tuple2(new StringValue("bb"), "gte"),
      Tuple2(new StringValue("bb"), "lt"),
      Tuple2(new StringValue("bb"), "gt"),
      Tuple2(new StringValue("bb"), "lt"),
      Tuple2(new StringValue("bb"), "gt"),
      Tuple2(new StringValue("bb"), "gt"),
      Tuple2(new StringValue("bb"), "lt"),
      Tuple2(new NumberValue(1.0), "eq"),
      Tuple2(new NumberValue(2.0), "lt"),
      Tuple2(new NumberValue(2.0), "lt"),
      Tuple2(new NumberValue(2.0), "lte"),
      Tuple2(new NumberValue(2.0), "lte"),
      Tuple2(new NumberValue(2.0), "gte"),
      Tuple2(new NumberValue(2.0), "gte"),
      Tuple2(new NumberValue(2.0), "gt"),
      Tuple2(new NumberValue(2.0), "gt")).foreach { case (v, op) =>
        mv.matches(v, op) shouldEqual(false)
    }
  }

  "FunctionValue" should "match nothing" in {
    match_nothing(new FunctionValue("foo", Seq()))
  }

  it should "resolve and evaluate the function against intrinsics" in {
    val r = new scala.util.Random()
    Seq("func0", "func1", "func2").foreach { name =>
      val arg0 = mock[IntrinsicValue]
      val argsN = (0 to r.nextInt(10)).map { _ => mock[IntrinsicValue] }
      val args = Seq(arg0) ++ argsN
      val rv = new NumberValue(1.0)
      val ctx = mock[Context]

      val fv = new FunctionValue(name, args)

      (arg0.apply_func _).expects(argsN, name).returning(Some(rv))

      val frv = fv.resolve(ctx)
      frv match {
        case Some(v) =>
          v shouldBe a [NumberValue]
          v.asInstanceOf[NumberValue].value shouldEqual(1.0)
        case None =>
          true shouldEqual(false)
      }
    }
  }

  it should "resolve to intrinsics via ResolveValue" in {
    val arg0 = mock[IntrinsicValue]
    val ref1 = mock[ReferenceValue]
    val arg1 = mock[IntrinsicValue]
    val arg2 = mock[IntrinsicValue]
    val rv = new StringValue("answer")
    val args = Seq(arg0, ref1, arg2)
    val name = "func0"
    val ctx = mock[Context]

    val fv = new FunctionValue(name, args)

    (arg0.apply_func _).expects(Seq(arg1, arg2), name).returning(Some(rv))
    (ref1.resolve _).expects(ctx).returning(Some(arg1))

    val forv = ResolveValue(fv, ctx)
    forv match {
      case Some(frv) => {
        frv shouldBe a [StringValue]
        frv.asInstanceOf[StringValue].value shouldEqual("answer")
      }
      case None => true shouldEqual(false)
    }
  }

  def map_to_expected(m: Map[String, String]): Map[String, Value] = {
    m.map { case (k, v) => (k, new StringValue(v)) }
  }

  // ValuesSection will be passed a null on mocking, not a None, so let's for it
  class MockableValuesSection extends ValuesSection(None) {}

  "SectionReferenceValue" should "lookup values" in {
    val ctx = mock[Context]
    val secs = mock[Sections]
    val sections = Map(
      "section0" -> Map("a0" -> "A", "b0" -> "B"),
      "section1" -> Map("a1" -> "AAA", "b1" -> "BBB"),
    )

    (ctx.sections _).expects().anyNumberOfTimes.returning(secs)

    sections.foreach { case (k, vals) =>
      val values = mock[MockableValuesSection]

      (secs.values _).expects(k).repeated(vals.size).times.returning(Some(values))
      vals.foreach { case (vk, v) =>
        val srv = new SectionReferenceValue(k, vk)
        (values.lookup _).expects(vk).returning(Some(new StringValue(v)))

        srv.resolve(ctx) match {
          case Some(iv) => {
            iv shouldBe a [StringValue]
            iv.asInstanceOf[StringValue].value shouldEqual(v)
          }
          case None => true shouldBe(false)
        }
      }
    }
  }

  it should "yield None for non-existent sections, keys" in {
    val ctx = mock[Context]
    val secs = mock[Sections]
    val empty_values = mock[MockableValuesSection]

    (ctx.sections _).expects().anyNumberOfTimes.returning(secs)
    (secs.values _).expects("empty").repeated(2).times.returning(Some(empty_values))
    (empty_values.lookup _).expects("a").returning(None)
    (empty_values.lookup _).expects("b").returning(None)
    (secs.values _).expects("missing").repeated(2).times.returning(None)

    Seq(
      new SectionReferenceValue("empty", "a"),
      new SectionReferenceValue("empty", "b"),
      new SectionReferenceValue("missing", "a"),
      new SectionReferenceValue("missing", "b"),
    ).foreach { srv => srv.resolve(ctx) shouldEqual(None) }
  }

  it should "keep values" in {
    val ctx = mock[Context]
    val secs = mock[Sections]
    val sections = Map(
      "section0" -> Map("a0" -> "A", "b0" -> "B"),
      "section1" -> Map("a1" -> "AAA", "b1" -> "BBB"),
    )

    (ctx.sections _).expects().anyNumberOfTimes.returning(secs)

    sections.foreach { case (k, vals) =>
      val values = mock[MockableValuesSection]

      (secs.values _).expects(k).repeated(vals.size).times.returning(Some(values))
      vals.foreach { case (vk, v) =>
        val srv = new SectionReferenceValue(k, vk)

        (values.retain _).expects(vk, *) onCall { (_: String, iv: IntrinsicValue) =>
          iv shouldBe a [StringValue]
          iv.asInstanceOf[StringValue].value shouldEqual(v)
          None
        }

        srv.update(ctx, new StringValue(v))
      }
    }
  }

  it should "not automatically create ValuesSections" in {
    val ctx = mock[Context]
    val secs = mock[Sections]

    (ctx.sections _).expects().anyNumberOfTimes.returning(secs)
    (secs.values _).expects("missing").repeated(4).times.returning(None)

    Seq(
      new SectionReferenceValue("missing", "a"),
      new SectionReferenceValue("missing", "b"),
    ).foreach { srv =>
      srv.update(ctx, new StringValue(s"${srv.key}_value"))
      srv.resolve(ctx) shouldEqual(None)
    }
  }

  it should "match equivalent references" in {
    val ref0 = new SectionReferenceValue("a", "x")

    ref0.matches(new SectionReferenceValue("a", "x"), "eq") shouldEqual(true)
    ref0.matches(new SectionReferenceValue("a", "y"), "eq") shouldEqual(false)
    ref0.matches(new SectionReferenceValue("b", "x"), "eq") shouldEqual(false)

    ref0.matches(new SectionReferenceValue("a", "x"), "lt") shouldEqual(false)
    ref0.matches(new SectionReferenceValue("a", "y"), "lt") shouldEqual(false)
    ref0.matches(new SectionReferenceValue("b", "x"), "lt") shouldEqual(false)

    ref0.matches(new SectionReferenceValue("a", "x"), "lte") shouldEqual(false)
    ref0.matches(new SectionReferenceValue("a", "y"), "lte") shouldEqual(false)
    ref0.matches(new SectionReferenceValue("b", "x"), "lte") shouldEqual(false)

    ref0.matches(new SectionReferenceValue("a", "x"), "gt") shouldEqual(false)
    ref0.matches(new SectionReferenceValue("a", "y"), "gt") shouldEqual(false)
    ref0.matches(new SectionReferenceValue("b", "x"), "gt") shouldEqual(false)

    ref0.matches(new SectionReferenceValue("a", "x"), "gte") shouldEqual(false)
    ref0.matches(new SectionReferenceValue("a", "y"), "gte") shouldEqual(false)
    ref0.matches(new SectionReferenceValue("b", "x"), "gte") shouldEqual(false)
  }

  "ResolveValue" should "resolve computed values" in {
    val cv = mock[ComputedValue]
    val iv = mock[IntrinsicValue]
    val ctx = mock[Context]

    (cv.resolve _).expects(ctx).returning(Some(iv))
    ResolveValue(cv, ctx) shouldEqual(Some(iv))
  }

  it should "resolve intrinsic values" in {
    val iv = mock[IntrinsicValue]
    val ctx = mock[Context]

    ResolveValue(iv, ctx) shouldEqual(Some(iv))
  }

  "ResolveManyValues" should "resolve many values, computed or intrinsic" in {
    val cv0 = mock[ComputedValue]
    val iv0 = mock[IntrinsicValue]
    val iv1 = mock[IntrinsicValue]
    val iv2 = mock[IntrinsicValue]
    val cv3 = mock[ComputedValue]
    val iv3 = mock[IntrinsicValue]
    val ctx = mock[Context]

    (cv0.resolve _).expects(ctx).returning(Some(iv0))
    (cv3.resolve _).expects(ctx).returning(Some(iv3))

    val vals = Seq(cv0, iv1, iv2, cv3)
    val ex_vals = Seq(Some(iv0), Some(iv1), Some(iv2), Some(iv3))

    ResolveManyValues(vals, ctx) shouldEqual(ex_vals)
  }

  "ResolveMapOfValues" should "resolve a Map[S, V] to Map[S, IV]" in {
    val cv0 = mock[ComputedValue]
    val iv0 = mock[IntrinsicValue]
    val iv1 = mock[IntrinsicValue]
    val iv2 = mock[IntrinsicValue]
    val cv3 = mock[ComputedValue]
    val iv3 = mock[IntrinsicValue]
    val ctx = mock[Context]

    (cv0.resolve _).expects(ctx).returning(Some(iv0))
    (cv3.resolve _).expects(ctx).returning(Some(iv3))

    val movs = Map(
      "a" -> cv0,
      "b" -> iv1,
      "c" -> iv2,
      "d" -> cv3
    )
    val ex = Map(
      "a" -> iv0,
      "b" -> iv1,
      "c" -> iv2,
      "d" -> iv3
    )

    ResolveMapOfValues(movs, ctx) shouldEqual(ex)
  }
}
