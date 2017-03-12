TEST_OBJECTS = sort_tests.beam permutations_tests.beam
OBJECTS = sort.beam permutations.beam

.PHONY: test

test: $(TEST_OBJECTS) $(OBJECTS)
	erl -noshell \
		-s eunit test sort \
		-s eunit test permutations \
		-s init stop

%.beam: %.erl
	erlc $<
