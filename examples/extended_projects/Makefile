all: ../subsystems/default.cgpr
	gprbuild -p new_ada_main.gpr --config=../subsystems/default.cgpr $(GPRBUILDFLAGS)

../subsystems/default.cgpr:
	$(MAKE) -C ../subsystems default.cgpr

clean: ../subsystems/default.cgpr
	gprclean -r new_ada_main.gpr --config=../subsystems/default.cgpr
	$(MAKE) -C ../subsystems clean


run: all
	./ada_main

.PHONY: all clean run
