all: default.cgpr
	gprbuild -p animals.gpr
	gprbuild -p except.gpr

default.cgpr:
	gprconfig --batch --config Ada  --config C++ -o $@

clean: default.cgpr
	gprclean -r animals.gpr
	gprclean -r except.gpr
	$(RM) default.cgpr

run: all
	./ada_main
	./cpp_main
	./main

.PHONY: all clean run
