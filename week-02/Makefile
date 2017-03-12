TEST_OBJECTS = sort_tests.beam
OBJECTS = sort.beam

.PHONY: test

test: $(TEST_OBJECTS) $(OBJECTS)
	erl -noshell -s eunit test sort -s init stop

%.beam: %.erl
	erlc $<
