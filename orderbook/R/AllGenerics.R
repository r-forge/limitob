## Generic functions of the orderbook class. There should be more
## documentation here. Is "AllGenerics" some sort of standard for S4
## packages?

.onLoad <- function(lib, pkg) require(methods)

## current.ob is a silly name

## Use and place index arguments similarly.

## current.time should be time. Why not POSIXct?

setClass("orderbook", representation(current.ob   = "data.frame",
                                     current.time = "numeric",
                                     file         = "character",
                                     file.index   = "numeric",
                                     trade.data   = "data.frame",
                                     my.trades    = "data.frame",
                                     animation    = "list",
                                     trader       = "logical",
                                     trade.index  = "numeric"
                                     ))


## Some of these are plausible methods. Others . . .

## Also, why is show() not generic? What about summary() and plot()?
## Why don't they need to be listed here? Do we need to list any of
## these?

if(!isGeneric("display"))
    setGeneric("display", function(object, n = 5, short = TRUE, ...)
               standardGeneric("display"))

if(!isGeneric("["))
    setGeneric("[", function(x, i)
               standardGeneric("["))

