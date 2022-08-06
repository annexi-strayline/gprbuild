all: default.cgpr
	gprbuild -p c_main.gpr $(GPRBUILDFLAGS)
	gprbuild -p ada_main.gpr $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Ada --config C

clean: default.cgpr
	gprclean -r c_main.gpr
	gprclean -r ada_main.gpr
	$(RM) default.cgpr

run: all
	./ada_main
	./c_main

.PHONY: all clean run
