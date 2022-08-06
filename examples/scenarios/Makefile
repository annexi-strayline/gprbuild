all: default.cgpr
	gprbuild -p main.gpr -XLIB=static $(GPRBUILDFLAGS)
	gprbuild -p main.gpr -XLIB=dynamic $(GPRBUILDFLAGS)


default.cgpr:
	gprconfig --batch --config Ada --config C

clean: default.cgpr
	gprclean -r main.gpr -XLIB=static
	gprclean -r main.gpr -XLIB=dynamic
	$(RM) default.cgpr

run: all
	./obj/main
	(PATH="../libraries/lib2:${PATH}" obj1/main)

.PHONY: all clean run
