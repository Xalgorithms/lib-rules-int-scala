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

  before {
    _ctx = mock[Context]
  }

  "FilterRefinement" should "exclude rows when the condition is empty" in {
    val r = new FilterRefinement(None)
    r.refine(_ctx, Map[String, IntrinsicValue]()) shouldEqual(None)
  }

  it should "exclude rows that do not pass the condition" in {
    val wh = mock[When]
    val r = new FilterRefinement(Some(wh))

    val row0 = Map("a" -> new StringValue("A"))
    val row1 = Map("b" -> new StringValue("B"))

    val rctx0 = new RowContext(_ctx, row0, null)
    val rctx1 = new RowContext(_ctx, row1, null)

    (wh.evaluate _).expects(rctx0).returning(false)
    (wh.evaluate _).expects(rctx1).returning(true)

    r.refine(rctx0, row0) shouldBe(None)
    r.refine(rctx1, row1) shouldBe(Some(row1))
  }

  "MapRefinement" should "yield the original if the assignment is None" in {
    val r = new MapRefinement(None)

    val row0 = Map("a" -> new StringValue("A"))
    val row1 = Map("b" -> new StringValue("B"))

    r.refine(_ctx, row0) shouldEqual(Some(row0))
    r.refine(_ctx, row1) shouldEqual(Some(row1))
  }

  it should "update the row" in {
    val ass = mock[Assignment]
    val r = new MapRefinement(Some(ass))

    val row0 = Map("a" -> new StringValue("A"))
    val row1 = Map("b" -> new StringValue("B"))

    val rctx0 = new RowContext(_ctx, row0, null)
    val rctx1 = new RowContext(_ctx, row1, null)

    val change0 = Map("a" -> new StringValue("AA"))
    val change1 = Map("c" -> new StringValue("C"))

    val ex0 = row0 ++ change0
    val ex1 = row1 ++ change1

    (ass.evaluate _).expects(rctx0).returning(change0)
    (ass.evaluate _).expects(rctx1).returning(change1)

    r.refine(rctx0, row0) shouldEqual(Some(ex0))
    r.refine(rctx1, row1) shouldEqual(Some(ex1))
  }
}
