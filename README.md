# nDm

Simple dice-rolling application written in Haskell. Supports being called in two ways:

1. With a single numeric argument, N, in which case it will roll Nd6 (that is, N six-sided dice).
2. With a format string that allows you to specify a modifier and the number of rolls. 

## Format string examples

The basic format is NdM(+|-Y)(xZ) where

* N is the number of dice per roll
* M is the number of sides per die
* Y is the optional modifier, preceded by either a plus or minus sign
* Z is the optional number of rolls to make

Examples:

* 3d6x2 - roll three 6-sided die twice.
* 4d4+1 - roll four 4-sided die and add one to the result
* 1d6-1 - roll one 6-sided die and substract one from the result
* 2d8+2x3 - roll two 8-sided die and add two to the result, three times.


