all: default.cgpr
	gprbuild -p matrix.gpr

default.cgpr:
	gprconfig --batch --config Ada  --config C --config Fortran -o $@

clean: default.cgpr
	gprclean matrix.gpr
	$(RM) default.cgpr

run: all
	./obj/main

.PHONY: all clean run
