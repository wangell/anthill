anthill
=======

anthill is a Logo-like language that generates turtle graphics in HTML5/Javascript via the canvas element.
It's programmed in Haskell using Attoparsec for lexing/parsing.

Usage
-----

`./anthill filename`

`./anthill Examples/pentagon-stripe.ah`

It is currently set to output to anthill.html

Language
--------

Built-in Functions

Binary Operators
+,-,\*

head [List]

Input: `head [1,2,3]`

Output: `1`

tail [List]
sum [List]

Turtle Commands

forward [expr]
backard [expr]
turn [expr]
pendown
penup
