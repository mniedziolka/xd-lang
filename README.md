# xd-lang

In this repository you can find an interpreter for a simple imperative language called "xd".
It was made for the "Programming languages and paradigms" course, which is an obligatory course at my university.
Because of that you can find here many inconsistencies or even errors.

## Licence
This project is released on the MIT license.
Feel free to use it in any way you like.
The whole license file can be found [here](./LICENSE).

## Running

### Machine with stack
```
stack build
stack run xd-lang prog.xd
```

## Implemented features
First:
* [x] 01 (3 types: bool, int, string)
* [x] 02 (literals, arithmetic, comparisons)
* [x] 03 (variables, assignments)
* [x] 04 (print)
* [x] 05 (while, if)
* [x] 06 (functions, recurrence)
* [x] 07 (passing the variable by reference or value)

Second
* [x] 09 (shadowing and static binding)
* [x] 10 (runtime error handling)
* [x] 11 (functions that can return a value)

Third:
* [x] 12 (4) (statically typed language)
* [x] 13 (2) (nested functions with static binding)
* [x] 16 (1) (break, continue)

## Examples
You can find some examples written in this language in the [examples/good](./examples/good) directory.
Their numbers are related to the list of implemented features.
There are also sam bad examples available in [examples/bad](./examples/bad) directory.
They're mostly checking the "Typechecker" functionalities.

## Inspirations
Some general concepts like code structure or managing the state of the program are taken from other projects.
Check them out, they might be useful:
* https://github.com/mbenke/toy-interpreters
* https://github.com/BAndysc/BInterpreter
* https://github.com/upicine/Capp-interpreter
* https://github.com/kkragoth-mimuw/lakke
