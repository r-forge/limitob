reset <- function(x){

    ## Clears current.ob from orderbook x. Does not clear trade data,
    ## my trades, or trade.index. Hmmmm. Necessary? Only used by
    ## read.time and read.orders.

    stopifnot(class(x) == "orderbook")

    current.ob <- data.frame(numeric(0), numeric(0),
                             character(0), numeric(0), character(0))

    names(current.ob) <- c("price", "size", "type", "time",
                           "id")

    x@current.time <- 0
    x@file.index <- 0
    x@current.ob <- current.ob

    invisible(x)
}


