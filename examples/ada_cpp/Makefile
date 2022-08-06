all: default.cgpr
	gprbuild animals.gpr $(GPRBUILDFLAGS)
	gprbuild except.gpr $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Ada --config C++

clean: default.cgpr
	gprclean animals.gpr
	gprclean except.gpr
	$(RM) default.cgpr

run: all
	./ada_main
	./cpp_main
	./main

.PHONY: all clean run
