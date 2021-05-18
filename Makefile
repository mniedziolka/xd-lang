build:
	stack build

build/grammar:
	make -C grammar/

run/examples: build
	for prog in examples/good/*.xd ; do \
		stack exec xd-exe $$prog || exit 1; \
	done

copy/grammar:
	cp grammar/build/Parser/*.hs src/Parser
	rm src/Parser/Test.hs