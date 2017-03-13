TEST_OBJECTS = sort_tests.beam permutations_tests.beam my_lists_tests.beam \
			   index_tests.beam
OBJECTS = sort.beam permutations.beam my_lists.beam index.beam

ERL_LIBS = ~/Projects/community/proper

.PHONY: test

test: $(TEST_OBJECTS) $(OBJECTS)
	ERL_LIBS=$(ERL_LIBS) erl -noshell \
		-s eunit test sort \
		-s eunit test permutations \
		-s eunit test my_lists \
		-s eunit test index \
		-s init stop

%.beam: %.erl
	ERL_LIBS=$(ERL_LIBS) erlc $<
