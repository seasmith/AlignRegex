AlignRegex
==========

[![Build Status](https://travis-ci.org/seasmith/AlignRegex.svg?branch=master)](https://travis-ci.org/seasmith/AlignRegex)

![Lifecycle](https://img.shields.io/badge/lifecycle-developing-red.svg)

<!-- ![GitHub release](https://img.shields.io/github/release/seasmith/AlignRegex.svg) -->
Align either pre-defined or user-input regular expressions. This is a port of [AlignAssign](https://github.com/seasmith/AlignAssign).

Main Demo
---------

![Main demo](inst/media/main_demo.gif)

Pre-Defined
-----------

### Align &lt;-

Nothing here, yet. This is also its own separate addin. ![]()

### Align =

Nothing here, yet. This is also its own separate addin. ![]()

### Align :

Nothing here, yet. ![]()

### Align AS

Nothing here, yet. ![]()

User-Input
----------

Nothing here, yet. ![]()

Behavior of commented-out regular expressions
---------------------------------------------

Be mindful that highling a chunk of code which has assignment operators within commented lines, like the following, and running the `Align <-` addin...

``` r
# This is a commented line with an assignment operator <-
a <- 1:5
b <- 6:10
c <- 11:15
# There is an assignment operator <- here, too
```

...will result in something like this.

``` r
# This is a commented line with an assignment operator <-
a                                                      <- 1:5
b                                                      <- 6:10
c                                                      <- 11:15
# There is an assignment operator                      <- here, too
```

Not so smart aligner
--------------------

There is also no special handling of assignment operators within a function. So, if you highlighted the entire chunk below and then ran the `Align <-` addin...

``` r
var1 <- letters
var2 <- as.list(sample(1:26, 26))
names(var2) <- var1[unlist(var2)]
list.pos <- function(name, lst){
    matches <- sapply(name, function(x){
        matched <- which(names(lst) %in% x)

        if(length(matched) == 0) matched <- NA
        matched
    })
    return(matches)
}
positions <- list.pos(c("a", "bbb", "c"), var2)
```

...the result will look like this.

``` r
var1                                     <- letters
var2                                     <- as.list(sample(1:26, 26))
names(var2)                              <- var1[unlist(var2)]
list.pos                                 <- function(name, lst){
    matches                              <- sapply(name, function(x){
        matched                          <- which(names(lst) %in% x)

        if(length(matched) == 0) matched <- NA
        matched
    })
    return(matches)
}
positions                                <- list.pos(c("a", "bbb", "c"), var2)
```
