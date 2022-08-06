all: default.cgpr
	gprbuild -p ft.gpr $(GPRBUILDFLAGS)

default.cgpr:
	gprconfig --batch --config Ada --config Fortran

clean: default.cgpr
	gprclean -r ft.gpr
	$(RM) default.cgpr

run: all
	./fobj/ess

.PHONY: all clean run
