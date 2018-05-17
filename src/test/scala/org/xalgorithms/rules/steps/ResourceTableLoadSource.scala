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
