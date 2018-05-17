// Copyright (C) 2018 Don Kelly <karfai@gmail.com>
// Copyright (C) 2018 Hayk Pilosyan <hayk.pilos@gmail.com>

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or (at
// your option) any later version.

// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
package org.xalgorithms.rules.steps

import org.xalgorithms.rules.{ LoadJsonTableSource }
import org.xalgorithms.rules.elements.{ PackagedTableReference }

import play.api.libs.json._
import scala.io.Source

class ResourceLoadTableSource extends LoadJsonTableSource {
  def read(ptref: PackagedTableReference): JsValue = {
    return Json.parse(Source.fromURL(getClass.getResource(s"/${ptref.name}.json")).mkString)
  }
}
