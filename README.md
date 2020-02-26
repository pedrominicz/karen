### Overview

```
?- yes.
yes.
?- no.
no.
?- zeros(X).
X = zero.
X = zero.
X = zero.
X = zero.
...
?- last(a,X).
X = cons(a, nil).
X = cons(_2, cons(a, nil)).
X = cons(_2, cons(_5, cons(a, nil))).
X = cons(_2, cons(_5, cons(_8, cons(a, nil)))).
...
?- last(X,cons(a,cons(b,cons(c,nil)))).
X = c.
```

- [x] Parser for a subset of Prolog is implemented.
- [x] Unification for simple terms.
- [x] Solver using the list monad for non-determinism.
- [x] Read-eval-print loop.
- [ ] Test.

### Useful Resources

http://propella.blogspot.com/2009/04/prolog-in-haskell.html

https://kyledewey.github.io/cs162w15/handouts/handout7-miniprolog.pdf

https://sites.cs.ucsb.edu/~kyledewey/cs162w17/handouts/handout8-miniprolog-smallstep.pdf
