Starting Point: A simple expression evaluator in Scala using
[parboiled2](https://github.com/sirthias/parboiled2) for parsing.

# Project 3a #

In this project, you will write a front end (lexical and syntax analysis combined as a PEG) for a Scala-based interpreter/REPL for the simple imperative language MiniC (you will work on the actual interpreter in project 3b and subsequent stages). Your recommended starting point is the [expressions-scala](https://github.com/LoyolaChicagoCode/expressions-scala) example.

- Extend grammar to support MiniJS's imperative constructs (statements)
- Extend abstract syntax tree to support the imperative constructs
- Extend the parser to support the imperative constructs
- Extend the unparser (pretty-printer) to support the imperative constructs

# Project 3b #

In this project, you will continue your work from project 3a by implementing the dynamic language semantics of MiniJS as part of the evaluator/interpreter. (This was disabled for project 3a because it did not yet handle the imperative constructs). Use the [simple imperative language](https://github.com/LoyolaChicagoCode/misc-scala/blob/master/src/main/scala/imperative) for additional guidance,

- Support [dynamic semantics](http://plone.cs.luc.edu/laufer-archived/teaching/473/handouts/SimpleImperative.html) of the new imperative constructs (statements)
- Create mutable store to map named variables to storage cells where values can be stored

# Project 3c #

In this project, you will add proper data structures (product or aggregation types) to the MiniJS language by adding constructs for:

    - structure creation
    - field selection
    - assignment to an existing field in a structure
    - addition of a new field to a structure

To this end, we will extend both the grammar and the interpreter. You may refer to this [structural operational semantics](http://plone.cs.luc.edu/laufer-archived/teaching/473/handouts/Records.html) example for a simpler version of this language (without field addition).

- Changes to the grammar
- Additions to the AST and unparser
- Support in the interpreter