experiment: ChineseRem.hs ChineseRem/IndepSet.hs ChineseRem/Set.hs Experiment.hs SetUtils.hs
	ghc --make -O2 Experiment.hs -o experiment

lca: ChineseRem.hs ChineseRem/IndepSet.hs ChineseRem/Set.hs LCA.hs
	ghc --make -O2 LCA.hs -o lca

clean:
	rm -vf *~ *.o *.hi experiment lca ChineseRem/*~ ChineseRem/*.o ChineseRem/*.hi