#Compile and Run - QC Gas Tax
cd ~/xalgo/xa-rules
bundle exec ruby cli.rb ~/xalgo/rules-interpreter/test-runs/qc-gas-tax/proximity-gas-tax.rule ~/xalgo/rules-interpreter/test-runs/qc-gas-tax/proximity-gas-tax.rule.json
echo "[info] [cnr] Compiled. Running..."
cd ~/xalgo/rules-interpreter
sbt "runMain org.xalgorithms.rules.Runner test-runs/qc-gas-tax/"
echo "[info] [cnr] Done."
