all: build/students

build:
	stack build

build/grammar:
	make -C grammar/

build/students:
	cp app/Main.hs src/
	cd src && ghc Main.hs -o interpreter
	mv src/interpreter .

run/examples: build
	for prog in examples/good/*.xd ; do \
		stack exec xd-exe $$prog || exit 1; \
	done

copy/grammar:
	cp grammar/build/Parser/*.hs src/Parser
	rm src/Parser/Test.hs