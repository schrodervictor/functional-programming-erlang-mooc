TEST_OBJECTS = shapes_tests.beam
OBJECTS = shapes.beam

.PHONY: test

test: $(TEST_OBJECTS) $(OBJECTS)
	erl -noshell -s eunit test shapes -s init stop

%.beam: %.erl
	erlc $<
