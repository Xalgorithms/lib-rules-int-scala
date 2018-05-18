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

class WhenSpec extends FlatSpec with Matchers with MockFactory {
  "When" should "evaluate simple values" in {
    val ctx = mock[Context]

    Seq("eq", "gt", "gte", "lt", "lte").foreach { op =>
      val v0 = mock[IntrinsicValue]
      val v1 = mock[IntrinsicValue]

      (v0.matches _).expects(v1, op).returning(false)
      (v1.matches _).expects(v0, op).returning(true)

      new When(v0, v1, op).evaluate(ctx) shouldEqual(false)
      new When(v1, v0, op).evaluate(ctx) shouldEqual(true)
    }
  }

  it should "evaluate ReferenceValues" in {
    val ctx = mock[Context]

    Seq("eq", "gt", "gte", "lt", "lte").foreach { op =>
      val v0 = mock[IntrinsicValue]
      val v1 = mock[IntrinsicValue]
      val rv0 = mock[ReferenceValue]
      val rv1 = mock[ReferenceValue]

      (rv0.resolve _).expects(ctx).returning(Some(v0)).twice
      (rv1.resolve _).expects(ctx).returning(Some(v1)).twice

      (v0.matches _).expects(v1, op).returning(false)
      (v1.matches _).expects(v0, op).returning(true)

      new When(rv0, rv1, op).evaluate(ctx) shouldEqual(false)
      new When(rv1, rv0, op).evaluate(ctx) shouldEqual(true)
    }
  }

  def validate_whens(vals: Seq[Boolean], expected: Boolean): Unit = {
    val ctx = mock[Context]

    EvaluateMany(ctx, vals.map { v =>
      val wh = mock[When]
      (wh.evaluate _).expects(ctx).returning(v)
      wh
    }) shouldEqual(expected)
  }

  it should "apply many whens" in {
    validate_whens(Seq(), true)
    validate_whens(Seq(true, true), true)
    validate_whens(Seq(true, false), false)
  }
}
