TEST_OBJECTS = shapes_tests.beam sum_bits_tests.beam
OBJECTS = shapes.beam sum_bits.beam

.PHONY: test

test: $(TEST_OBJECTS) $(OBJECTS)
	erl -noshell -s eunit test shapes -s init stop
	erl -noshell -s eunit test sum_bits -s init stop

%.beam: %.erl
	erlc $<
