# Copyright (C) 2018 Don Kelly <karfai@gmail.com>
# Copyright (C) 2018 Hayk Pilosyan <hayk.pilos@gmail.com>

# This file is part of Interlibr, a functional component of an
# Internet of Rules (IoR).

# ACKNOWLEDGEMENTS
# Funds: Xalgorithms Foundation
# Collaborators: Don Kelly, Joseph Potvin and Bill Olders.

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU Affero General Public License
# as published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Affero General Public License for more details.

# You should have received a copy of the GNU Affero General Public
# License along with this program. If not, see
# <http://www.gnu.org/licenses/>.
require 'multi_json'
require 'xa/rules/parse'

include XA::Rules::Parse::Content

Dir.glob("#{ARGV[0]}/*.rule") do |ifn|
  ofn = "#{ifn}.json"
  puts "> compiling #{ifn} to #{ofn}"
  IO.write(ofn, MultiJson.dump(parse_rule(IO.read(ifn)), pretty: true))
end

Dir.glob("#{ARGV[0]}/*.table") do |ifn|
  ofn = "#{ifn}.json"
  puts "> compiling #{ifn} to #{ofn}"
  IO.write(ofn, MultiJson.dump(parse_table(IO.read(ifn)), pretty: true))
end
