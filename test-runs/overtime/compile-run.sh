#RCF- Compile and run - Overtime
rm ~/xalgo/rules-interpreter/test-runs/overtime/overtime.rule.json
cd ~/xalgo/xa-rules
bundle exec ruby cli.rb ~/xalgo/rules-interpreter/test-runs/overtime/overtime.rule ~/xalgo/rules-interpreter/test-runs/overtime/overtime.rule.json
echo "Compiled. Running..."
cd ~/xalgo/rules-interpreter
sbt "runMain org.xalgorithms.rules.Runner test-runs/overtime/"
echo "Done."
