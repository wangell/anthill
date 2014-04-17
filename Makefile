anthill: Anthill.hs Parser.hs Evaluator.hs Codegen.hs
	ghc --make Anthill.hs -o anthill
	rm *.o
	rm *.hi
