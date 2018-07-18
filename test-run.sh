#!/bin/bash
ls test-runs/$1/*.rule | while read f; do
    n=`basename $f`
    d=`dirname $f`
    bundle exec ruby compile.rb $f "$d/$n.json"
done
sbt "runMain org.xalgorithms.rules.Runner test-runs/$1"
