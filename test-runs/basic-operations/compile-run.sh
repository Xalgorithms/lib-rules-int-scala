#RCF- Compile and run - Overtime
rm ~/xalgo/rules-interpreter/test-runs/basic-operations/basic-operations.rule.json
cd ~/xalgo/xa-rules
bundle exec ruby cli.rb ~/xalgo/rules-interpreter/test-runs/basic-operations/basic-operations.rule ~/xalgo/rules-interpreter/test-runs/basic-operations/basic-operations.rule.json
echo "Compiled. Running..."
cd ~/xalgo/rules-interpreter
sbt "runMain org.xalgorithms.rules.Runner test-runs/basic-operations/"
echo "Done."
