inside.market <- function(x){

    ## Returns a data frame with a row for the best ask and a row for the
    ## best bid.  The columns are price, size, and type. Should this be structured differently?

    stopifnot(class(x) == "orderbook")

    ask <- best(x, side = "ASK")
    bid <- best(x, side = "BID")

    ## Returns the inside market as an object

    invisible(rbind(ask, bid))

}

                                        # Seems horribly redundant!
                                        # Just give inside.market an
                                        # argument which allows for
                                        # "bid", "ask" or "both."

best <- function(x, side){

    ## Returns a named vector with the price and size of the best bid
    ## or ask in orderbook x. Perhaps ought to return this data in
    ## some other fashion?

    side <- toupper(side)
    stopifnot(side %in% c("ASK", "BID"))
    stopifnot(class(x) == "orderbook")


    ## Pull out current.ob into x.

    x <- x@current.ob

    ## Takes out the correct side

    x <- x[x[["type"]] == side,]

    if(nrow(x) == 0){

        return(NA)

    } else {

        ## Gets indices for the best bid or ask. Is there some cooler
        ## way of doing this? Only difference is max for bid and min
        ## for ask.

        if(side %in% "BID"){
            index <- which(x[["price"]] == max(x[["price"]]))
        }
        else{
            index <- which(x[["price"]] == min(x[["price"]]))
        }

        ## Any of the indices would get us the best price, so we
        ## just use the first one. Better way?

        price <- x[["price"]][index[1]]

        ## Sum to find the total size at the indices.

        size <- sum(x[["size"]][index])

        ## Return named vector of the best bid/ask price and
        ## total size at that price level.

        return(c(price = price, size = size))
    }

}



mid.point <- function(x){

    ## Returns the midpoint value, which is just the simple average of
    ## the best bid and ask. Note that we get rid of the names that
    ## are associated with best(). Why would you ever want the hassle
    ## of dealing with named vectors in something like this?

    stopifnot(class(x) == "orderbook")

    return(as.numeric((best(x, side = "BID")[1] + best(x, side = "ASK")[1])/2))

}



spread <- function(x){

    ## Returns the spread.
                                        # Combine with midpoint?
                                        # Test cases?

    stopifnot(class(x) == "orderbook")

    ## Gets best bid and best ask.

    ask = best(x, side = "ASK")
    bid = best(x, side = "BID")

    ## If there are either no bids or no asks, then return
    ## NA. Otherwise return the difference of the best ask
    ## and best bid. Need an NA test case. Ought to be a
    ## cleaner way to code this.

    if(is.na(ask) || is.na(bid)){

        return(NA)

    } else{

        return(ask[["price"]] - bid[["price"]])

    }

}
