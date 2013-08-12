lambda-calculus
===============

A lambda calculus interpreter

Here's an example of one beta reduction:

```
cabal-dev install
echo "((λ a (a a)) b)" | ./dist/build/lc/lc
(b b)
```
