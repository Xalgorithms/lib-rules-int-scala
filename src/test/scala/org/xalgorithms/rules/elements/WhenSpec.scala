// Copyright 2018 Don Kelly <karfai@gmail.com>

// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License. You may
// obtain a copy of the License at

// http://www.apache.org/licenses/LICENSE-2.0

// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied. See the License for the specific language governing
// permissions and limitations under the License.

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
