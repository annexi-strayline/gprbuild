all: ../subsystems/default.cgpr
	gprbuild -p new_ada_main.gpr --config=../subsystems/default.cgpr

../subsystems/default.cgpr:
	make -C ../subsystems default.cgpr

clean: ../subsystems/default.cgpr
	gprclean -r new_ada_main.gpr --config=../subsystems/default.cgpr


run: all
	./ada_main

.PHONY: all clean run
