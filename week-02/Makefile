TEST_OBJECTS = sort_tests.beam permutations_tests.beam my_lists_tests.beam
OBJECTS = sort.beam permutations.beam my_lists.beam

.PHONY: test

test: $(TEST_OBJECTS) $(OBJECTS)
	erl -noshell \
		-s eunit test sort \
		-s eunit test permutations \
		-s eunit test my_lists \
		-s init stop

%.beam: %.erl
	erlc $<
