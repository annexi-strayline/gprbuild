all: default.cgpr
	gprbuild -p ft.gpr $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Ada --config Fortran -o $@

clean: default.cgpr
	gprclean -q -r ft.gpr
	$(RM) default.cgpr

run: all
	./fobj/ess

.PHONY: all clean run
