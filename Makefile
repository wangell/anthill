anthill: Anthill.hs Parser.hs Evaluator.hs Codegen.hs
	ghc --make Anthill.hs
	rm *.o
	rm *.hi
