MCC=/usr/local/MATLAB/R2013a/bin/mcc
MCCFLAGS=

TESTS=engine runtime generic
default: $(TESTS)
	@echo "All tests should have produced:"
	@echo "  ready"
	@echo "  MXClassDouble"
	@echo "  -1.0"
	@for t in $^ ; do echo Running $$t... ; ./$$t ; done

%: %.hs
	ghc -I.. --make $@

lib%.so: %.m
	$(MCC) $(MCCFLAGS) -W lib:$(basename $@) -T link:lib $^

runtime: libmtest.so

clean:
	rm -rf libmtest_mcr
	rm -f *.o *.hi $(TESTS) libmtest* mccExcludedFiles.log readme.txt
