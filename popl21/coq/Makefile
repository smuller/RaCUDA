all: calc.vo cost.vo eval.vo logic.vo cost-block.vo

calc.vo: calc.v
	coqc calc.v

cost.vo: calc.vo cost.v
	coqc cost.v

eval.vo: calc.vo cost.vo eval.v
	coqc eval.v

logic.vo: calc.vo cost.vo eval.vo logic.v
	coqc logic.v

cost-block.vo: calc.vo cost.vo eval.vo logic.vo cost_block.v
	coqc cost_block.v

assumptions:
	coqc assumptions.v

clean:
	rm -f *.vo
	rm -f *.vok
	rm -f *.vos
	rm -f *.glob
	rm -f trace
