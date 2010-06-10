################################################################################
##
## $Id: AllGenerics.R 1300 2010-06-08 21:01:11Z zhao $
##
## Generic functions for the limitob class
##
################################################################################

.onLoad <- function(lib, pkg) require(methods)

setClass("orderbook", representation(current.ob = "data.frame",
                                     current.time = "numeric",
                                     ob.names = "character"))

if(!isGeneric("spread"))
    setGeneric("spread", function(object, ...) standardGeneric("spread"))

if(!isGeneric("get.order.info"))
    setGeneric("get.order.info", function(object, id, ...)
               standardGeneric("get.order.info"))

if(!isGeneric("snapshot"))
    setGeneric("snapshot", function(object, new.time,...)
               standardGeneric("snapshot"))

if(!isGeneric("display"))
    setGeneric("display", function(object, n = 5, short = TRUE, ...)
               standardGeneric("display"))

if(!isGeneric("ask.price.levels"))
    setGeneric("ask.price.levels", function(object, ...)
               standardGeneric("ask.price.levels"))

if(!isGeneric("bid.price.levels"))
    setGeneric("bid.price.levels", function(object, ...)
               standardGeneric("bid.price.levels"))

if(!isGeneric("total.price.levels"))
    setGeneric("total.price.levels", function(object, ...)
               standardGeneric("total.price.levels"))

if(!isGeneric("bid.orders"))
    setGeneric("bid.orders", function(object, ...)
               standardGeneric("bid.orders"))

if(!isGeneric("ask.orders"))
    setGeneric("ask.orders", function(object, ...)
               standardGeneric("ask.orders"))

if(!isGeneric("total.orders"))
    setGeneric("total.orders", function(object, ...)
               standardGeneric("total.orders"))

if(!isGeneric("mid.point"))
    setGeneric("mid.point", function(object, ...)
               standardGeneric("mid.point"))

if(!isGeneric("inside.market"))
    setGeneric("inside.market", function(object, invis = FALSE, ...)
               standardGeneric("inside.market"))

if(!isGeneric("add.order"))
    setGeneric("add.order", function(object, price, size, type, time=NULL, 
               id = NULL, ...) standardGeneric("add.order"))

if(!isGeneric("best.bid"))
    setGeneric("best.bid", function(object, ...)
			   standardGeneric("best.bid"))

if(!isGeneric("best.ask"))
    setGeneric("best.ask", function(object, ...)
	           standardGeneric("best.ask"))

if(!isGeneric("remove.order"))
    setGeneric("remove.order", function(object, id, ...)
               standardGeneric("remove.order"))

if(!isGeneric("replace.order"))
    setGeneric("replace.order", function(object, id, size, ...)
               standardGeneric("replace.order"))

if (!isGeneric("market.order"))
    setGeneric("market.order", function(object, size, type, ...)
               standardGeneric("market.order"))

if (!isGeneric("market.order.price"))
    setGeneric("market.order.price", function(object, size, price, ...)
               standardGeneric("market.order.price"))

if (!isGeneric("snapshot"))
    setGeneric("snapshot", function(object, new.time, show = TRUE, ...)
               standardGeneric("snapshot"))
