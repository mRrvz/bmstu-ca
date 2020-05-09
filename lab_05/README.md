# Численное интегрирование

**Кто клонит реп и не ставит лайк — тот пидарас!**

Все вопросы в отчёте.

Таблица значений - table.csv

# Usage

```
make run
```

Сомневаюсь, что у кого-то получится хотя бы запустить программу.

Зависимости, помимо ghc: polynomial (cabal install polynomial)

В самой программе стоит интегрирование **два раза Гауссом**, чтобы поменять на **Симпсон + Гаусс** заменить 

```
putStrLn "Enter M: " >> fmap (\x -> read x :: Int) getLine >>= print . gauss2 limits tau n
```

на

```
putStrLn "Enter M: " >> fmap (\x -> read x :: Int) getLine >>= print . simpson2 limits tau n
```

в файле Main.hs
