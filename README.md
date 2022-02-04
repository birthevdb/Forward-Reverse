# Forward-Reverse
Forward or Reverse Mode Automatic Differentiation: What's the Difference?

* `expressions.hs` contains a simple expression language, and some example expressions.
* `abstract.hs` contains the abstract algebraic data structures: semiring, d-module, d-algebra, Clifford-Weil and Kronecker,
as well as a generic, single-line differentiation function.
* `symbolic.hs` shows how to symbolically derive an expression.
* `forward.hs` shows how we can instantiate the abstract function in order to implement forward-mode automatic differentiation,
together with an optimized version using sparse maps.
* `reverse.hs` shows how we can instantiate the abstract function in order to implement reverse-mode automatic differentiation,
together with an optimized version using mutable arrays.
* `overloading` shows how, by means of naturality, we eliminate the basic expression.
* `sharing.hs` shows how, by means of let-constructs, we share sub-expressions.
* `streams.hs` shows how, by means of streams, we compute higher-order derivatives.
