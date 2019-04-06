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

class DefinitionsSpec extends FlatSpec with Matchers with MockFactory with BeforeAndAfter {
  var _ctx: Context = null

  before {
    _ctx = mock[Context]
  }

  "Assignment" should "evaluate into a Map" in {
    val cv = mock[ComputedValue]
    val iv = new StringValue("BB")

    val vals = Map(
      "a" -> new StringValue("AA"),
      "b" -> cv)
    val ex_vals = Map(
      "a" -> vals("a"),
      "b" -> iv)

    (cv.resolve _).expects(_ctx).returning(Some(iv))

    vals.foreach { case (k, v) =>
      val rv = new SectionReferenceValue("_local", k)
      val ass = new Assignment(rv, v)
      val m = ass.evaluate(_ctx)

      m.contains(k) shouldBe(true)
      m(k) shouldEqual(ex_vals(k))
    }
  }

  it should "yield empty map when the Value resolves to None" in {
    val cv = mock[ComputedValue]

    (cv.resolve _).expects(_ctx).returning(None)

    val ass = new Assignment(new SectionReferenceValue("_local", "x"), cv)
    ass.evaluate(_ctx) shouldEqual(Map[String, IntrinsicValue]())
  }
}
