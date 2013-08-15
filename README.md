lambda-calculus
===============

A lambda calculus interpreter

Here's an example of 10 beta reductions of the y combinator:

```
cabal-dev install
echo "((λ f ((λ b (f (b b))) (λ b (f (b b))))) x)" | ./dist/build/lc/lc ReduceNTimes 10
0.  ((λ f ((λ b (f (b b))) (λ b (f (b b))))) x)
1.  ((λ b (x (b b))) (λ b (x (b b))))
2.  (x ((λ b (x (b b))) (λ b (x (b b)))))
3.  (x (x ((λ b (x (b b))) (λ b (x (b b))))))
4.  (x (x (x ((λ b (x (b b))) (λ b (x (b b)))))))
5.  (x (x (x (x ((λ b (x (b b))) (λ b (x (b b))))))))
6.  (x (x (x (x (x ((λ b (x (b b))) (λ b (x (b b)))))))))
7.  (x (x (x (x (x (x ((λ b (x (b b))) (λ b (x (b b))))))))))
8.  (x (x (x (x (x (x (x ((λ b (x (b b))) (λ b (x (b b)))))))))))
9.  (x (x (x (x (x (x (x (x ((λ b (x (b b))) (λ b (x (b b))))))))))))
10. (x (x (x (x (x (x (x (x (x ((λ b (x (b b))) (λ b (x (b b)))))))))))))
```
