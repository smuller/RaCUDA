include ../config.mk

all: Driver.native
	mv Driver.native ../$(TOOL_NAME)
%.cmo %.mli %.byte %.native: force
	-@CXX=$(CXX) ocamlbuild -use-ocamlfind -cflag -w -cflag "-0..67" -lflag str.cmxa -no-hygiene $@
	cp -r frontc _build; \
	cd _build; \
	ocamlopt.opt -w "-0..67" unix.cmxa str.cmxa -ccopt -g -g Clp_Stubs.o -cc c++ -cclib ../../packages/sandbox/lib/libClp.a -cclib ../../packages/sandbox/lib/libCoinUtils.a -cclib -lbz2 -cclib -lz -I lib -I frontc Clp.cmx Config.cmx Types.cmx Utils.cmx DNF.cmx Dist_Types.cmx Dist.cmx Polynom.cmx Focus_Types.cmx Focus_Builder.cmx Focus.cmx Graph_Types.cmx lib/print.cmx Presburger.cmx lib/hashhe.cmx lib/ilist.cmx lib/sette.cmx lib/mappe.cmx lib/pHashhe.cmx lib/pSette.cmx lib/pMappe.cmx lib/sHGraph.cmx lib/pSHGraph.cmx lib/fixpointType.cmx lib/time.cmx lib/fixpointStd.cmx lib/fixpointDyn.cmx lib/fixpointGuided.cmx lib/fixpoint.cmx CUDA_Config.cmx frontc/cabs.cmx CUDA.cmx Graph_AI_Simple.cmx IMP_Types.cmx IMP_Grammar.cmx IMP_Lexer.cmx IMP.cmx Graph.cmx Neededness.cmx Analysis.cmx CS_Interop_Types.cmx CS_Interop.cmx CS_Interop_Grammar.cmx CS_Utils.cmx CS_Interop_Lexer.cmx CS_Querier_CFG.cmx CUDA_Types.cmx IMP_Simpl.cmx CUDA_Cost.cmx Coqgen.cmx Graph_Reader.cmx Heuristics.cmx InterIR.cmx Cs_conversion.cmx frontc/clexer.cmx frontc/cparser.cmx frontc/cxml.cmx frontc/ctoxml.cmx frontc/frontc.cmx CUDA_Eval.cmx CUDA_Params.cmx Driver.ml -o ../Driver.native

clean:
	rm -fr ../$(TOOL_NAME) *.native *.byte _build

force: ;

.PHONY: all clean force
