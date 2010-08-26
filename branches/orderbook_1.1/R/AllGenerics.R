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


## Is all this gibberish necessary?

if(!isGeneric("spread"))
    setGeneric("spread", function(object, ...) standardGeneric("spread"))

if(!isGeneric("display"))
    setGeneric("display", function(object, n = 5, short = TRUE, ...)
               standardGeneric("display"))

if(!isGeneric("mid.point"))
    setGeneric("mid.point", function(object, ...)
               standardGeneric("mid.point"))

if(!isGeneric("["))
    setGeneric("[", function(x, i)
               standardGeneric("["))


## Ought to be invisible.

if(!isGeneric("inside.market"))
    setGeneric("inside.market", function(object, invis = FALSE, ...)
               standardGeneric("inside.market"))

if(!isGeneric("best.bid"))
    setGeneric("best.bid", function(object, ...)
			   standardGeneric("best.bid"))

if(!isGeneric("best.ask"))
    setGeneric("best.ask", function(object, ...)
	           standardGeneric("best.ask"))

if(!isGeneric("reset"))
    setGeneric("reset", function(object)
               standardGeneric("reset"))

if(!isGeneric("read.orders"))
    setGeneric("read.orders", function(object, n = 1000) #Why that default?
               standardGeneric("read.orders"))

if(!isGeneric("read.time"))
    setGeneric("read.time", function(object, t) #Why t?
               standardGeneric("read.time"))

## Not sure that I like these argument names.

if(!isGeneric("load.animation"))
    setGeneric("load.animation", function(object, from, to, fps = 1,
                                         by = "sec", bounds = 0.02)
               standardGeneric("load.animation"))

if(!isGeneric("load.trade.animation"))
    setGeneric("load.trade.animation", function(object, tradenum,
                                                before = 30, after =
                                                30, fps = 1, by = "both", bounds
                                                = 0.02)
               standardGeneric("load.trade.animation"))

if(!isGeneric("animate"))
    setGeneric("animate", function(object, by = "sec", start = NULL,
                                   end = NULL, pause = 0.25, initPause
                                   = 2)
               standardGeneric("animate"))

if(!isGeneric("initialize.trades"))
    setGeneric("initialize.trades", function(object, time = c(5, 300))
               standardGeneric("initialize.trades"))

if(!isGeneric("load.next.trade"))
    setGeneric("load.next.trade", function(object, before = 30, after
                                           = 30, fps = 1, by = "both",
                                           bounds = 0.02)
               standardGeneric("load.next.trade"))

if(!isGeneric("load.previous.trade"))
    setGeneric("load.previous.trade", function(object, before = 30, after
                                           = 30, fps = 1, by = "both",
                                           bounds = 0.02)
               standardGeneric("load.previous.trade"))



