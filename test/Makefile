GHC = ghc -Wall

# DO NOT DELETE: Beginning of Haskell dependencies
test_switch : test_switch.hs
	${GHC} --make test_switch

test_aa : test_aa.hs
	${GHC} --make test_aa
# DO NOT DELETE: End of Haskell dependencies

clean:
	rm -f test_aa test_switch *.o *.hi
