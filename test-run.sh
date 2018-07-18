#!/bin/bash
bundle exec ruby compile.rb "test-runs/assemble/$1.rule" test-runs/assemble/$1.rule.json
sbt "runMain org.xalgorithms.rules.Runner test-runs/$1"
