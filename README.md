# ScalaRegexImplementation
A regex engine written in Scala. A tutorial by https://github.com/rcoh/toyregex .

## Compilation and Running

run the command `sbt run` at project's root directory.

## The regular expression language
* `.`: Match any character
* `|`: Match `abc` OR `cde`
* `+`: Match one or more of the previous pattern
* `*`: Match 0 or more of the previous pattern
* `(` and `)`: grouping

## Plan of attack
Regular expression will be evaluated in 3 phases:
1. Parse the regular expression into a syntax tree
2. Convert the syntax tree into a state machine
3. Evaluate the state machine against our string

The machine called [NFA](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton)
is used to evaluate regular expression. As the state machine consumes inputs,
it will move from state to state. If the state machine
got to a point where we can't follow an allowed transition, the
regular expression doesn't match the string. The time complexity of the state machine is 
O( length(input) * length(expression) ).
