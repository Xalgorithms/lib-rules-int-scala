#Compile and Run - Overtime
rm ~/xalgo/rules-interpreter/test-runs/overtime/overtime.rule.json
cd ~/xalgo/xa-rules
bundle exec ruby cli.rb ~/xalgo/rules-interpreter/test-runs/overtime/overtime.rule ~/xalgo/rules-interpreter/test-runs/overtime/overtime.rule.json
echo "[info] [cnr] Compiled. Running..."
cd ~/xalgo/rules-interpreter
sbt "runMain org.xalgorithms.rules.Runner test-runs/overtime/"
echo "[info] [cnr] Done."
