#Compile and Run - Overtime
rm ~/xalgo/rules-interpreter/test-runs/basic-operations/basic-operations.rule.json
cd ~/xalgo/xa-rules
wd=`pwd`
echo "[info] [cnr] Compiling from $wd"
bundle exec ruby cli.rb ~/xalgo/rules-interpreter/test-runs/basic-operations/basic-operations.rule ~/xalgo/rules-interpreter/test-runs/basic-operations/basic-operations.rule.json
echo "[info] [cnr] Compiled. Running..."
cd ~/xalgo/rules-interpreter
sbt "runMain org.xalgorithms.rules.Runner test-runs/basic-operations/"
echo "[info] [cnr] Done."
