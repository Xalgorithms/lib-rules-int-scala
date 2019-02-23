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

import com.github.javafaker.Faker
import org.scalamock.scalatest.MockFactory
import org.scalatest._

import org.xalgorithms.rules._
import org.xalgorithms.rules.elements._

class RefinementsSpec extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {
  var _ctx: Context = null
  val _faker = new Faker

  val _table = Seq(
    Map("a" -> new NumberValue(2.0), "b" -> new StringValue("B")),
    Map("a" -> new NumberValue(4.0), "b" -> new StringValue("C")),
    Map("a" -> new NumberValue(1.0), "b" -> new StringValue("A")),
    Map("a" -> new NumberValue(5.0), "b" -> new StringValue("E")),
    Map("a" -> new NumberValue(3.0), "b" -> new StringValue("D"))
  )

  val _empty_table: Seq[Map[String, IntrinsicValue]] = Seq()

  before {
    _ctx = mock[Context]
  }

  "FilterRefinement" should "exclude rows when the condition is empty" in {
    val r = new FilterRefinement(None)
    _table.foreach(r.add(new RowContext(_ctx, Map[String, IntrinsicValue](), null), _))
    r.refine() shouldEqual(_empty_table)
  }

  it should "exclude rows that do not pass the condition" in {
    val wh = mock[When]
    val r = new FilterRefinement(Some(wh))
    val ex_table = Seq(
      _table(0),
      _table(2),
      _table(3)
    )

    val rctxs = _table.map(new RowContext(_ctx, _, null))
    val when_rets = Seq(
      true,
      false,
      true,
      true,
      false
    )
    val verify_when_context = (rctx: RowContext, row: Map[String, IntrinsicValue], ret: Boolean) => {
      (call_ctx: Context) => {
        call_ctx shouldBe a [RowContext]
        call_ctx.asInstanceOf[RowContext].local_row shouldEqual(row)
        ret
      }
    }

    rctxs.zipWithIndex.foreach { case (rctx, i) =>
      (wh.evaluate _).expects(*) onCall verify_when_context(rctx, _table(i), when_rets(i))
    }

    _table.zip(rctxs).foreach { case (row, rctx) => r.add(rctx, row) }
    r.refine() shouldEqual(ex_table)
  }

  "MapRefinement" should "yield the original if the assignment is None" in {
    val r = new MapRefinement(None)

    _table.foreach(r.add(new RowContext(_ctx, Map[String, IntrinsicValue](), null), _))
    r.refine() shouldEqual(_table)
  }

  it should "update the row" in {
    val ass = mock[Assignment]
    val r = new MapRefinement(Some(ass))
    val rctxs = _table.map(new RowContext(_ctx, _, null))

    val elems = Seq("00", "11", "22", "33", "44").map { v => Map("c" -> new StringValue(v)) }
    val ex_table = elems.zip(_table).map { case (elem, row) =>
      row ++ elem
    }

    val verify_assignment_context = (
      rctx: RowContext,
      row: Map[String, IntrinsicValue],
      elem: Map[String, IntrinsicValue]
    ) => {
      (call_ctx: Context) => {
        call_ctx shouldBe a [RowContext]
        call_ctx.asInstanceOf[RowContext].local_row shouldEqual(row)

        elem
      }
    }

    rctxs.zipWithIndex.foreach { case (rctx, i) =>
      (ass.evaluate _).expects(*) onCall verify_assignment_context(rctx, _table(i), elems(i))
    }

    _table.zip(rctxs).foreach { case (row, rctx) => r.add(rctx, row) }
    r.refine() shouldEqual(ex_table)
  }

  "ConditionalTakeRefinement" should "exclude rows when the condition is empty" in {
    val r = new ConditionalTakeRefinement(None)
    _table.foreach(r.add(new RowContext(_ctx, Map[String, IntrinsicValue](), null), _))
    r.refine() shouldEqual(_empty_table)
  }

  it should "exclude rows that do not pass the condition" in {
    val wh = mock[When]
    val r = new ConditionalTakeRefinement(Some(wh))

    val ex_table = Seq(
      _table(1),
      _table(3),
      _table(4)
    )

    val rctxs = _table.map(new RowContext(_ctx, _, null))
    val when_rets = Seq(
      false,
      true,
      false,
      true,
      true
    )
    val verify_when_context = (rctx: RowContext, row: Map[String, IntrinsicValue], ret: Boolean) => {
      (call_ctx: Context) => {
        call_ctx shouldBe a [RowContext]
        call_ctx.asInstanceOf[RowContext].local_row shouldEqual(row)
        ret
      }
    }

    rctxs.zipWithIndex.foreach { case (rctx, i) =>
      (wh.evaluate _).expects(*) onCall verify_when_context(rctx, _table(i), when_rets(i))
    }

    _table.zip(rctxs).foreach { case (row, rctx) => r.add(rctx, row) }
    r.refine() shouldEqual(ex_table)
  }

  "FunctionalTakeRefinement" should "exclude rows when the condition is empty" in {
    val r = new FunctionalTakeRefinement(None)
    _table.foreach(r.add(new RowContext(_ctx, Map[String, IntrinsicValue](), null), _))
    r.refine() shouldEqual(_empty_table)
  }

  it should "exclude rows when an unknown function is used" in {
    (0 to _faker.number().numberBetween(2, 10)).foreach { _ =>
      val r = new FunctionalTakeRefinement(Some(new TakeFunction(_faker.lorem().word())))
      _table.foreach(r.add(new RowContext(_ctx, Map[String, IntrinsicValue](), null), _))
      r.refine() shouldEqual(_empty_table)
    }
  }

  def verify_take_functional(index: Int, count: Int, fn: TakeFunction) = {
    val ex_table = _table.slice(index, count)
    val r = new FunctionalTakeRefinement(Some(fn))

    _table.foreach { row => r.add(new RowContext(_ctx, row, null), row) }
    r.refine() shouldEqual(ex_table)
  }

  it should "include the first N rows using first(N)" in {
    val count = _faker.number().numberBetween(1, _table.length)
    val fn = new TakeFunction(
      "first",
      Seq(new NumberValue(BigDecimal(count)))
    )

    verify_take_functional(0, count, fn)
  }

  it should "refine nothing in failure cases for first" in {
    val cases = Seq(
      Seq(new StringValue("asdasdasd")),
      Seq(): Seq[Value]
    )

    cases.foreach { c =>
      val fn = new TakeFunction("first", c)
      val r = new FunctionalTakeRefinement(Some(fn))
      _table.foreach { row => r.add(new RowContext(_ctx, row, null), row) }
      r.refine() shouldEqual(_empty_table)
    }
  }

  it should "include a subset of N rows using nth(index, N)" in {
    val index = _faker.number().numberBetween(0, _table.length - 1)
    val count = _faker.number().numberBetween(1, _table.length)

    val fn = new TakeFunction(
      "nth",
      Seq(
        new NumberValue(BigDecimal(index)),
        new StringValue(count.toString)
      )
    )

    verify_take_functional(index, count + 1, fn)
  }

  it should "refine nothing in failure cases for nth" in {
    val cases = Seq(
      Seq(new StringValue("asdasdasd")),
      Seq(new NumberValue(1), new StringValue("asdasdasd")),
      Seq(new NumberValue(1)),
      Seq(): Seq[Value]
    )

    cases.foreach { c =>
      val fn = new TakeFunction("nth", c)
      val r = new FunctionalTakeRefinement(Some(fn))
      _table.foreach { row => r.add(new RowContext(_ctx, row, null), row) }
      r.refine() shouldEqual(Seq())
    }
  }

  it should "include the last N rows using last(N)" in {
    val count = _faker.number().numberBetween(1, _table.length)
    val fn = new TakeFunction(
      "last",
      Seq(new NumberValue(BigDecimal(count)))
    )

    verify_take_functional(_table.length - count, _table.length, fn)
  }

  // TODO: test with non-numerical
  // TODO: test with references
  // TODO: test with other functions (like add)
}
