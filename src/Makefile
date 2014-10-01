MCC=mcc
MCCFLAGS=

libhsmatlab.so: hsmatlab.m hsmatlab.c
	rm -f $@ $@.post
	# just create the wrapper
	$(MCC) $(MCCFLAGS) -W lib:$(basename $@) -c hsmatlab.m
	# matlab 7.6 sticks the ctf at the end of the library, but has a bug when not creating libraries that it tries anyway, so we check that here
	-[ -f $@ ] && mv $@ $@.post
	# stick our own call into the wrapper
	cat hsmatlab.c >> $(basename $@).c
	# build the library
	$(MCC) $(MCCFLAGS) -W lib:$(basename $@) -T link:lib $(basename $@).c
	# 7.6: do what mcc tried to do before
	-[ -f $@.post ] && cat $(basename $@).ctf >> $@ 

clean:
	rm -rf libhsmatlab_mcr
	rm -f libhsmatlab* mccExcludedFiles.log readme.txt
