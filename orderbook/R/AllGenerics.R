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

## Do any of these really need to be methods?

if(!isGeneric("read.orders"))
    setGeneric("read.orders", function(object, n = 1000) #Why that default?
               standardGeneric("read.orders"))

if(!isGeneric("read.time"))
    setGeneric("read.time", function(object, t) #Why t?
               standardGeneric("read.time"))

if(!isGeneric("load.animation"))
    setGeneric("load.animation", function(object, from, to, fps = 1,
                                         by = "sec", bounds = 0.02)
               standardGeneric("load.animation"))

if(!isGeneric("animate"))
    setGeneric("animate", function(object, by = "sec", start = NULL,
                                   end = NULL, pause = 0.25, initPause
                                   = 2)
               standardGeneric("animate"))
