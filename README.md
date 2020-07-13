# coding-challenges

This repository contains implementations of [TheCodingTrain's](https://www.youtube.com/user/shiffman)
[Coding Challenges](https://youtu.be/17WoOqgXsRM?list=PLRqwX-V7Uu6ZiZxtDDRCi6uhfTH4FilpH) in Haskell. In these
videos Daniel Shiffman (author of the channel) tries to solve some programming problem in **10 minutes** using
[Processing](https://processing.org/) or [p5.js](https://p5js.org/). I highly recommend to visit his channel even if
you're a pro in programming - content is still quite entertaining.

## Implementation

All examples use [the Gloss library](http://hackage.haskell.org/package/gloss) for graphics, which is great for
creating small and simple projects. It's pretty simple and it doesn't use advanced things in Haskell like
[monads](https://wiki.haskell.org/Monad), [applicative functors](https://wiki.haskell.org/Applicative_functor) or
[arrows](https://wiki.haskell.org/Arrow). This makes it very beginner-friendly. There's a great tutorial
[here](http://andrew.gibiansky.com/blog/haskell/haskell-gloss/) about making Pong using this library.

## Notes

### Configuration

All examples include constants that configure example and can be changed.

### Coordinates

Gloss uses the following coordinate system:

             ^
           y |
             |      x
    <--------|-------->
     -x      |
             | -y
             v

Instead of typical one (Processing uses it):

             ^
           y |
             |      x
    <--------|-------->
    -x       |
             | -y
             v
