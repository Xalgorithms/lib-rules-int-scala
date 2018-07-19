#!/bin/bash
bundle exec ruby compile.rb $1
sbt "runMain org.xalgorithms.rules.Runner test-runs/$1"
