all: default.cgpr
	gprbuild -p -Pc_main
	gprbuild -p -Pada_main

default.cgpr:
	gprconfig --batch --config Ada --config C -o $@

clean:
	gprclean -r -Pc_main
	gprclean -r -Pada_main
	$(RM) default.cgpr

run: all
	./ada_main
	./c_main

.PHONY: all clean run
