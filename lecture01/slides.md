# Lecture one

!SLIDE

# Lecture one
## Oh my God, it's pure!

### alfredod@cakesolutions.net

!SLIDE

## Haskell for the Scala programmer

* We won't focus too much on trivialities, because the audience is the
  novice and/or seasoned functional programmer

![haskellandscala](images/haskell-scala.png)

!SLIDE left

## Haskell at glance

* Haskell is a *purely functional programming language*
* Lazy by default
* Strongly typed
* Compiled but interpretable if needed

!SLIDE

## Purely functional

![Enemy state](images/enemy_state.jpg)

!SLIDE left

* Haskell *enforce* purity, making mutation **really** hard by "default":
  * Less bugs
  * Code reusability
  * Plays nice with concurrency and parallelism

!SLIDE

### A taste of Haskell

* Haskell has acquired the reputation of being a language with a steep learning curve

* This is due to the nature of the language (purity above all) and because
  what is sometimes one of the most trivial program may seems puzzling at first

!SLIDE

#### Hello world in Python

``` python
if __name__ == "__main__":
  print "Hello World!"
```

#### Hello world in Scala

``` scala
object Main extends App {
  println("Hello World")
}
```

#### Hello world in Haskell

``` haskell
module Main where

main :: IO ()
main = print "Hello World"
```

!SLIDE

![wat](images/wat.jpg)

!SLIDE

### What is IO () ?
* One of the biggest error in teaching Haskell is starting from the I/O,
namely from something **impure**

* We'll start the other way around, from **pure** (mathematical and non) functions

!SLIDE

### Assignments are pure!

* Haskell is **really** pure, so it takes assignments pretty seriously:

``` haskell
x = 10
x = 5 -- error!
```

* Assignment is defined, in Haskell, in a mathematical sense:
* We defined ```x``` to be ```10```, and the line after we said it is ```5```
  * Nonsense!
* In a certain extend, we can think of it how Scala's ```val```

!SLIDE

### Functions
* Due to its mathematical nature, defining functions in Haskell is a breeze:
  * Question: what do you think this program is gonna do?

``` haskell
square :: Int -> Int
square x = x * x
```

**Note**: We could omit the type signature, but it's a good habit to include
it: It helps you reason about the code, making obvious what a function is
supposed to do.

!SLIDE
* One really cool thing is that you can "stack" definition for the same
  function, defining what a function should do for a *particular* input. This
  allows us to write concise code such as:

``` haskell
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

**Note**: In theory, we could do even more!

``` haskell
factorial n = product [1..n] -- more on that later!
```

!SLIDE

## Lazy by default

![lazy bears](images/lazy.jpg)

!SLIDE left

* Haskell is lazy; an expression won't be evaluated until it doesn't to

* This allows us to do pretty cool stuff, making the code easier to reason about

!SLIDE

### Ranges

* We can succinctly create a list specifying a range of numbers:

``` haskell
[1..5] -- will create [1,2,3,4,5]
```

**But!** being lazy allows us to write something like this:

``` haskell
take 3 [0..] --yield [0,1,2]
```

* We generated **all the naturals numbers**! This is possible because Haskell
  won't evaluate that infinite sequence, it will only takes the first three elements
  out of the list!

!SLIDE

## Strongly typed

* As Scala programmer, we already know the advantages of being strongly typed,
  so we won't dig more inside this last point

![strongly typed](images/strongly-typed.png)

!SLIDE left

## Compiled but interpretable if needed

* Haskell ecosystem is rich:
* The [Haskell Platform](http://www.haskell.org/platform/) is a convenient distribution which includes:
  * **GHC**, the _Glasgow Haskell Compiler_
  * **Cabal**, the _Haskell Package Manager_
  * **Ghci**, the _Haskell Interpreter_

!SLIDE left

* We can compile our programs directly with GHC, with:

``` shell
ghc myfile.hs
```

* ... or we can invoke Ghci to play around:

``` shell
GHCi, version 7.6.1: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> 2 + 2
4
```

!SLIDE left

## Complementary tools

* [Hackage](http://hackage.haskell.org/packages/hackage.html), a site hosting thousands of high-quality haskell packages (installable with Cabal)

* [Hayoo](http://holumbus.fh-wedel.de/hayoo/hayoo.html) and [Hoogle](http://www.haskell.org/hoogle/),
  two terrific tools for searching functions, also via type signature

!SLIDE left

## Simple homework

1. Go on Hayoo or Hoogle and search for ```map``` (hint, is the first returned result).

2. Familiar? Is the same ```map``` we also have in Scala

3. Write a function: 

``` haskell
squareList :: Int -> [Int]
```

That using the formerly defined ```square```, returns a list of ```n```
squared elements (hint: the starting list must be from 1 to n).

