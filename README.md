anthill
=======

anthill is a Logo-like language that generates turtle graphics in HTML5/Javascript via the canvas element.
The interpreter is programmed in Haskell using Attoparsec for lexing/parsing.

Usage
-----

`anthill` is the interpreter

`./anthill filename`

`./anthill Examples/pentagon-stripe.ah`

It is currently set to output to anthill.html

Language
--------

###Primitives###

Integers : `2`

Characters : `'c'`

Lists : `[1,2,3]`

Strings (lists of characters): `"hello"` == `['h','e','l','l','o']`

###Binary Operators###

`+ 2 3`

`(((- 4 5)))`

`head [1,2,3]`

`tail [expr]`

`sum [expr]`

###Turtle Commands###

`forward (expr)`

`backard (expr)`

`turn (expr)`

`pendown`

`penup`
