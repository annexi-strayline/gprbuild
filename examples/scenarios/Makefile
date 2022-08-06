all: default.cgpr
	gprbuild -p main.gpr -XLIB=static
	gprbuild -p main.gpr -XLIB=dynamic


default.cgpr:
	gprconfig --batch --config Ada  --config C -o $@

clean:
	gprclean -r main.gpr -XLIB=static
	gprclean -r main.gpr -XLIB=dynamic
	$(RM) default.cgpr

run: all
	./obj/main
	(PATH="../libraries/lib2:${PATH}" obj1/main)

.PHONY: all clean run
