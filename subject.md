# GLADOS_

< GENERIC LANGUAGE AND DATA OPERAND

SYNTAX/>


**GLADOS**

DELIVERY METHOD: GitHub

**Preliminaries**

```
binary name: glados
language: haskell
compilation: via Makefile, including re, clean and fclean rules
build tool: free (stack recommanded)
```
```
3 The totality of your sourcefiles, except all uselessfiles (binary, tempfiles, objfiles,...), must
be included in your delivery.
3 All the bonus files (including a potential specific Makefile) should be in a directory named
bonus.
3 Error messages have to be written on the error output, and the program should then exit with
the 84 error code (0 if there is no error).
```
The goal of this project is to implement a programming language of your own design, in Haskell. Don't
panic just yet, we will go there step by step.

The project will be split in different parts that are all mandatory. Read them with attention, they are de-
signed to help you progress on a smooth difficulty curve.

Also for your trouble, cake and grief counseling will be available at the conclusion of the test.


Part 0 : The Enrichment Testing Center

```
The Enrichment Center promises to always provide a safe testing environment. In dangerous test-
ing environments, the Enrichment Center promises to always provide useful advice. For instance:
division by zero will kill your program. Try to avoid it.
```
Tests are not only useful, they prevent you from shooting yourself in the foot when adding more code.
To be sure you develop a perfectly fine language, the GLaDOS system requires you to test **thouroughly**
your code.

**Build system and dependencies**

You are free to use any build system you see fit, but you **must** use one ( **stack** is always a safe choice).
During each defense you will be asked to demonstrate how to build your project from scratch (i.e from a
fresh git clone).

```
Dependencies and functions
```
Your are available to use your **own parsing library** or one of the following libraries: **parsec** , **megaparsec**.

Also, you are free to use any library you want for the other parts of the project.

Also, even if there is no automatic check and therefor the coding style will not be verified as stricly as
usual, the base principles of clean code still applies, and **unsafe function** and **mutable** constructs are still
**stricly forbidden**.

```
If you use good practices, your code will be easier to understand, split, reuse and test.
```

```
Unit & Integration tests
```
You are free to use any unit testing framework you want, but you need one and you **must** have a compre-
hensive testing policy, including unit and integration test suits.

You **must** be able to show how much of your code is covered by tests.

**Continuous Integration and Continious Delivery**

Finally, to end this part on the wonders of testing, we'll ask you to automate everything by building a CI /
CD.
You must choose the CI of your choice to automate the process of testing.
A good idea for example would be to test each part of the code **before** each push to prevent from pushing
bad code.

To satisfy the CD part of the requirement, your system must produce a fully functional release build auto-
matically (including a functioning executable).

```
Part 0 is absolutely mandatory for every defense
Failure to comply will result in an 'unsatisfactory' mark on your official testing record followed by
death (of your program but still).
```

Part 1 : Lots of Irritating Superfluous

Parentheses (LISP)

For thisfirst part of the project you **must** implement a minimalist **LISP** interpreter. To be more precise,
when not specified otherwise in this document, your language must behave just likeChez-Scheme. It will
be evaluated during thefirst Defense of this project.

In the rare occasion of you being one of those humans who think he's faster than everyone else, and you
are already at part 2 before even thefirst defense, you **MUST** still have the LISP clone code available (on
a separate branch of your git, or as an option of your latest build).

Failure to do so will result in you **not having cake** at yourfirst defense, nor points nor credits either.
Also, be sure to read the following warning. Repetition is key.

```
Part 0 is absolutely mandatory for every defense
Failure to comply will result in an 'unsatisfactory' mark on your official testing record.
```
**Syntax**

Being a LISP, thisfirst language **must** be represented bySymbolic-Expressions. At the bare minimum your
parser **must** be able to handle:

```
3 Atoms:
```
**-** Signed Integers in base 10
**-** Symbols (any string which is not a number)
3 Lists:
**-** started by an open parenthesis, ended by a close parenthesis
**-** contains zero, one or any number of sub-expressions separated by spaces, tabs, carriage re-
turns or parenthesis.


```
Examples of valid S-Expressions:
```
```
foo
```
```
42
```
```
(1 2 3)
((foo bar baz)
(1 2 3) ()
((((1(2) 3))))
)
```
**Invocation and error handling**

Your program **must** be able to read your language code from standard input. You are free to add more and
fancier way to invoke it (fromfiles given as arguments, with a full featured REPL, etc.).

```
∇ Terminal - + x
$>cat foo.scm
(define foo 21)
(* foo 2)
$>./glados < foo.scm
42
```
You must stop the execution as soon as an error occurs and return a 84 status code. You're free to display
any meaningful information on the standard output or error output.

```
∇ Terminal - + x
$>cat error.scm
(* foo 2)
$>./glados < error.scm
*** ERROR : variable foo is not bound.
$>echo $?
84
```

**Core concepts**

At the bare minimum your program **must** handle the following concepts:

```
Types
```
```
3 You language must support 64 bit integers and boolean values.
3 Boolean values must be represented by the ”#t” and ”#f” symbols, for True and False respectively.
3 As a consequence of being a functional language, it must also support a procedure type (more infor-
mation about that bellow)
```
Optionally, you are free to implement more types (for example: lists).

```
Bindings
```
You **must** implement a way to **define** an association (binding) between a symbol and a value (which can
be seen as having variables, to be able to store them and reuse them afterwards). When a symbol is bound
to a value, it evaluates to this value. Trying to evaluate an unbound symbol produces an error.

The **define** notation is a special form which binds an expression to a symbol:

```
(define <SYMBOL> <EXPRESSION>)
```
```
∇ Terminal - + x
> foo
*** ERROR : variable foo is not bound.
> (define foo 42)
> foo
42
```

```
User defined functions and lambda
```
You **must** define a way to represent (and call) functions, being anonymous ones ( **lambdas** ) or named ones.
Functions **must** be able to take parameters (or none).

A function call (application) is simply a list with the callee (the operator) placed in first place, the other
elements of the list being the arguments (the operands). This is the default behavior when a list doesn't
match with a special form.

```
∇ Terminal - + x
$>cat call.scm
(div 10 2)
$>./glados call.scm
5
```
**Lambdas**
A lambda is a special form composed of a (possibly empty) list of parameters and a body.
The body is an expression, which will be evaluated when the lambda is called, within a context where the
parameters will take the values of the arguments provided during invocation.

A lambda has the following form:

```
(lambda (<ARG1> <ARG2> ... <ARGN>) <BODY>)
```
```
∇ Terminal - + x
$>cat lambda1.scm
(lambda (a b) (+ a b))
$>./glados < lambda1.scm
#\<procedure\>
$>cat lambda2.scm
((lambda (a b) (+ a b)) 1 2)
$>./glados < lambda2.scm
3
$>cat lambda3.scm
(define add
(lambda (a b)
(+ a b)))
(add 3 4)
$>./glados < lambda3.scm
7
```

**Named functions**
In this language named functions are just syntactic sugar added to the **define** notation. The following
example produces the same result as **”lambda3.scm”** above:

```
(define (<FUNC_NAME> <ARG1> <ARG2> ... <ARGN>) <BODY>)
```
```
∇ Terminal - + x
$>cat function1.scm
(define (add a b)
(+ a b))
(add 3 4)
$>./glados < function1.scm
7
```
```
Named functions must be capable to call themselves, to allow recursion.
```
```
Conditional expressions
```
You **must** define a way to represent conditions using a **if** notation, which contains a conditional expression
followed by two more expressions. Thefirst of these expressions is evaluated and returned if the condition
is true, otherwise the second is evaluated and returned.

It **must** have the form:

```
(if <CONDITION> <THEN> <ELSE>)
```
Where CONDITION, THEN and ELSE are three arbitrarily complex sub-expressions.


```
∇ Terminal - + x
$>cat if1.scm
(if #t 1 2)
$>./glados < if1.scm
1
$>cat if2.scm
(if #f 1 2)
$>./glados < if2.scm
2
$>cat if3.scm
(define foo 42)
(if (< foo 10)
(* foo 3)
(div foo 2))
$>./glados < if3.scm
21
```
```
You are free to implement the cond notation too but it's not mandatory.
```
```
Builtin functions
```
You've seen some of them already in the examples, but in order to make your language (barelly) usable,
you **must** implement some functions which will be hardcoded in your interpreter:

```
3 Predicates, which take two arguments and evaluates to a boolean value:
```
**- ”eq?”** (returns true if its two arguments are equal, false otherwise)
**- ”<”** (returns true if thefirst argument is strictly lower than the second, false otherwise)
3 Aritmethic operators, which take two arguments and return an integer:
**- ”+”** , **”-”** , **”*”
- ”div”** (division) and **”mod”** (modulo)

```
Even if they are represented by a single special character, they are just symbols bound to a function
and behave like any user defined functions.
```

∇ Terminal - + x
$>cat builtins1.scm
(+ (* 2 3) (div 10 2))
$>./glados < builtins1.scm
11
$>cat builtins2.scm
(eq? (* 2 5) (- 11 1))
$>./glados < builtins2.scm
#t
$>cat builtins3.scm
(< 1 (mod 10 3))
$>./glados < builtins2.scm
#f


**Examples**

Given all the rules above, Your lisp interpreter should be able, for example, to process the following pro-
grams:

```
∇ Terminal - + x
$>cat superior.scm
(define (> a b)
(if (eq? a b)
#f
(if (< a b)
#f
#t)))
(> 10 -2)
$>./glados < superior.scm
#t
$>cat factorial.scm
(define (fact x)
(if (eq? x 1)
1
(* x (fact (- x 1)))))
(fact 10)
$>./glados < factorial.scm
3628800
```
```
Think about your code structure, and data structure. You will probably reuse some parts of your
code for the Part
```

Part 2 : embrace and extend

At this point you should have a minimal but functional core language.

Your goal for this second and last part is to make it evolve into a more advanced language on four axis:

```
3 Language-based security and robustness (skill: security )
3 Its syntax, grammar and semantics (skill: parsing )
3 The way and how fast it executes code (skill: evaluation / compilation )
3 Documentation and accessibility (skill: documentation )
```
#warn(To reach a good grade your project must feature improvements on each of these axis compared to
Part 1.)

```
Additionaly, to reach the maximum level of credits (bonuses), your project must also implement
more general features (types supported, data structures, input/outputs, FFI, etc.)
```
**Language-based security and robustness**

Your language is a powerful tool, but with great power comes great responsibility. You must implement
some security features to prevent your users from shooting themselves in the foot.

Your reflexion should be based on the following questions:
* What is security in a programming language?
* Are my types safe?
* How can you prevent your users from doing stupid things?

```
Think about how you can make you language safer, your choice must be justified and explained in
your documentation.
```

**Syntax, grammar and semantics**

Symbolic expressions are neat but all these parenthesis can be hard to parse for a human eye, and they
limit the expressiveness of your language. Remove your S-Expression parser and implement your own
syntax (or keep the LISP front-end and add a new frontend with your own language).

```
To be crystalclear: your grammar can't be based on s-expressions. If it does you will fail the parsing
skills. If in doubt, ask your local staff (earlier is better)
```
You have a lot of freedowm in the form you can give to your language. Nevertheless, you still have to follow
some rules, and you're advised to not try to reinvent the wheel: you're encouraged to get your inspiration
from the languages you already know.

Your language can't be line oriented. We ask you to implement a high level language, not a glorified assem-
bler. While some carriage returns may be mandatory in your grammar, any expression must be splitable
on several lines. And once again, your language can't be formed of s-expressions.

Accecibilityisalsoakeypointinthispart. Youmustprovideacohérentandclearsyntax, andyourlanguage
must be easy to read and understand.

During thefinal defense we will look for several features of your language, implement them to get a better
grade:

```
3 A formal document describing your language's grammar (you're encouraged to use BNF ). This part
is mandatory to validate.
3 A consistent, unclutered grammar. for example:
```
**-** ifyourlanguagegeneratesmoreparenthesisthanLISP,maybeyoutookabadturnsomewhere...
**-** if your language use curly brackets and explicit return statement (just like C) but is otherwise
    not imperative, maybe there's a more concise way to represent your code.
3 Some syntactic sugar (two (or more) syntactic forms which result in the same AST)
3 Infix arythmetic operators with the expected priorities (rembember the EvalExpr?)
3 Infix user defined operators with user defined priority (as in Haskell or Kaleidoscope for example)


**Evaluation and compilation**

If you went for the simplest solution in **Part1** , you should now have a basic AST walking, environment
passing interpreter. It'sfine for afirst try but you can do better.

```
3 You must implement a Virtual Machine , with its own Instruction Set to execute your language pro-
grams.
```
```
3 Your VM instruction set must represent a function as a flat list of instructions (no recursive / self-
referential constructs inside functions)
```
```
3 You must implement a Compiler capable of translating a program from your language into a form
suitable for your VM (the compiler and VM can be two modules inside the same executable, or two
binaries).
```
```
3 Your compiler should be capable of displaying its output in the form of human readable text (dissas-
sembly).
```
```
3 Your compiler should be able to output it's result as a binary format (bytecode).
```
```
3 Your VM should be able to load this binary format and run it.
```
```
3 Your could include in your delivery a standard library, coded in your language, extending the basic
functionalities of your language.
```
```
3 Your language could support closures.
```
```
Closures are not trivial to support, it's a high risk / high reward goal... Choose wisely.
```
```
Your program must be able to handle errors and exceptions
```

**Documentation and accessibility**

You must provide a comprehensive documentation of your language. This documentation **must** include:

```
3 A user manual, explaining how to use your language, with examples.
3 A formal description of your language's grammar (you're encouraged to use BNF ).
3 A description of the compilation process.
3 A review of the languages you took inspiration from, from a security point of view (or lack therof).
3 A description of the security features you implemented, in relation with your review.
3 Optionaly, a developer manual, explaining how to extend your language.
```
Your need to make your documentation accessible for everyone. Think about the people who use tools
like screen readers, or who have difficulties reading small or complex fonts. Additionaly, if your langage
presentspecificaccessibilityissuesorfeatures, youmustdiscussorhighlighttheminyourdocumentation.

```
Your documentation is a key part of your project. The quality of your documentation will be taken
into account in yourfinal grade.
```
Resources

```
3 Related to Part 1 :
```
**-** The Roots of Lisp
**-** The Art of the Interpreter(part 0 and part 1, part 2 for advanced topics)
3 Related to **Part 2** :
**-** 500 Lines or Less | A Python Interpreter Written in Python
**-** Code Generation(advanced)
**-** Null-References


Bonus

In order to unlock the maximum level of credits, you must implement more features in your language. We
give you a lot of freedom here, but they must correspond to one of the main ”tracks” defined here:

```
3 More data types:
```
**-** Floating point numbers (with corresponding division operator: / )
**-** Symbols as data (as in LISP using quote, or as in Erlang/Elixir, or Prolog)
**-** Lists
    * with corresponding builtins to manipulate them: cons/car/cdr or (:)/head/tail
    * syntactic sugar for literals, for example: [1,2,3]
**-** Strings (as lists of Char or with a custom set of builtins to use them)
**-** Tuples, Structures, Arrays, HashMaps? etc.
3 Side effects:
**-** add builtins or a notation to read or write to standard input / output
**-** add builtins or a notation forfile handling
**-** bindings to graphical library, or networking primitives, etc.
**-** a notation to interface with any external function (Foreign Function Interface)
3 Type inference:
**-** being able to detect before execution if a program / function is not properly typed
**-** provide a notation to anotate functions / expressions with types
3 Additionnal backends:
**-** extend your compiler to output bytecode for an existing plateform (Java, C#, WebAssembly,
Erlang, Python...)
**-** extend your compiler to output native binary code for x86_64, ARM, RiscV, M68k, MOS-
or any real hardware (you can use bindings to LLVM to implement this, or handle the codegen
part yourself)
3 Additionnal runtime:
**-** write a second VM in a different language (this will only be evaluated if the Haskell VM is fully
functional)
3 Metaprogrammation:
**-** provide notations to allow the user to programmaticaly modify the program structure
**-** for example: LISP macros, Rust macros, Elixir macros...
3 Imperative constructs:
**-** capability to describe functions as sequence of statements
**-** mutable variables
**-** loops
3 Optimisation
**-** TCO (tail call optimisation)
**-** anything that make your code run faster (don't forget the benchmarks to prove it!)


- v 2.


