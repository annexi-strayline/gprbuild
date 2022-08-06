all: default.cgpr
	gprbuild -p c_main.gpr
	gprbuild -p ada_main.gpr

default.cgpr:
	gprconfig --batch --config Ada --config C -o $@

clean: default.cgpr
	gprclean -r c_main.gpr
	gprclean -r ada_main.gpr
	$(RM) default.cgpr

run: all
	./ada_main
	./c_main

.PHONY: all clean run
