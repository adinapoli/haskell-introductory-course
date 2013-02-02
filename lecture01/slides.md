# Lecture one

!SLIDE

# Lecture one
## Oh my God, it's pure!

### alfredod@cakesolutions.net

!SLIDE left

## Haskell for the Scala programmer

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
* We defined ```x``` to be ```10```, and the line after we said it was ```5```
  * Nonsense!
* In a certain extend, we can think of it how Scala's ```val```

!SLIDE

### Functions
* Due to its mathematical nature, defining functions in Haskell is a breeze:
  * Question: what do you think this program is gonna do?

``` haskell
square :: Double -> Double
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
