#!/bin/bash
bundle exec ruby compile.rb $1
if [ $? -ne 0 ]; then
  echo "Compile failure!"
  exit -1
fi
sbt "runMain org.xalgorithms.rules.Runner $1"
rm $1/*.rule.json
rm $1/*.table.json
