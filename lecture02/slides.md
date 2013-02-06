# lecture02

!SLIDE

# Lesson 02: Down the Haskell's hole

## Yet another way to introduce the basic syntax

### alfredod@cakesolutions.net

!SLIDE

## Haskell syntax for the Scala hacker

* Haskell syntax is clean and almost clutter free, so it doesn't
  take long to master
* We must only be sure to make really clear some fundamental differences
  with Scala

![walrus](images/walrus.jpg)

!SLIDE

### A small taste

* There is no concept of "object":

#### Scala

``` scala
List(1,2,3) map { _ * 2 } //calling the map() method of List() object
```

#### Haskell

``` haskell
map (*2) [1,2,3]
```

In other terms, functions are first-class citizens!

!SLIDE

## Our menu

* Conditional branching
* Lists
* Ranges
* List comprehensions
* Tuples
* More on functions

!SLIDE

### Conditional branching

Unlike Scala and most programming language, Haskell ```if/else``` must be
a complete block; in other terms, you **cannot omit the else branch!**

* That's because **everything** in Haskell is an expression, something which
  returns a value. Because the ```else``` is mandatory, an if
  statement will always return something, and that's why is an expression.
