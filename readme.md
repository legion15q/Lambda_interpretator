# Интерпретатор лямбда исчисления. Способен редуцировать лямбда терм (в т.ч. рекурсивный). 
### Пример использования
#### Из кода: 
```
print $ evalWhile [] (App (Lam "x" (App (Var "x")(Var "x")))(Lam "x" (Var "x")))
print $ eval [] (Lam "x" (App (Var "x")(Var "x")))
print $ eval [] (App (Var "x")(Var "x"))
print $ eval [] (Lam "x" (Var "x"))
```
#### Консоль:
```
- \x(x\(x))
- \x(xx)
- \x(x)
```
