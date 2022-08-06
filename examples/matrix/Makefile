all: default.cgpr
	gprbuild -p

default.cgpr:
	gprconfig --batch --config Ada  --config C --config Fortran -o $@

clean:
	gprclean
	$(RM) default.cgpr

run: all
	./obj/main

.PHONY: all clean run
