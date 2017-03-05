# Week 1

## Assignment: Shapes

I took this assignment as an opportunity to sharpen my skills with records
manipulation and geometrical calculations in the 2D plane, using Erlang.

All the records can be found in the `shapes.hrl` file.

To make the problem more challenging, it was solved by arbitrary geometric
shapes defined by its points in the 2D plane. The supported shapes are:

- square
- rectangle
- circle
- triangle

The public API of the `shapes` module is totally covered by EUnit tests in the
`shapes_tests` module.

The module is shipped with a `Makefile` to make compilation and running the
tests easier. One just need to `cd` into this directory and run:

    $ make test
