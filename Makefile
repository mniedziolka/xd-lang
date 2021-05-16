build:
	stack build

build/grammar:
	make -C grammar/

copy/grammar:
	cp grammar/build/Parser/*.hs src/Parser
	rm src/Parser/Test.hs