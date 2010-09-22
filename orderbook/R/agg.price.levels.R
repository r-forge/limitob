agg.price.levels <- function(x, bounds, kind){

    ## Helper function that returns a data frame with the
    ## shares/orders aggregated by price level and with data outside
    ## bounds % on either side of the midpoint removed. Takes an
    ## orderbook object as input. Returns a data frame with price,
    ## size, and type. (Is type a good name for ASK/BID?) Mainly
    ## (only?) used in plotting.

    stopifnot(kind %in% c("orders", "shares"))

    ## Save the midpoint. Necessary?

    mid <- mid.point(x)

    x <- x@current.ob

    stopifnot(all(c("price", "size", "type") %in% names(x)))

    ## Removes rows bounds % above and below the midpoint. Best way to
    ## do this? Seems to work OK.

    stopifnot(bounds > 0)

    x <- x[x[["price"]] < mid*(1 + bounds) &
           x[["price"]] > mid*(1 - bounds),]

    stopifnot(nrow(x) > 0)

    ## Aggregate by price level. Check for NA? Must be a cooler way of
    ## doing this.

    if(kind %in% "shares"){
        x <- aggregate(x[["size"]], by = list(x[["price"]]), sum)
        names(x) <- c("price", "shares")
    } else {
        x <- aggregate(x[["size"]], by = list(x[["price"]]), length)
        names(x) <- c("price", "orders")
    }

    ## Rows with price above midpoint are ask, price below midpoint
    ## are bid.

    x$type[x[["price"]] > mid] <- "ASK"
    x$type[x[["price"]] < mid] <- "BID"

    ## Sort for ease of use and clean up row names.

    x <- x[order(x$price, decreasing = TRUE),]

    row.names(x) <- 1:nrow(x)

    return(x)
}

## New version for use with animation, will merge this with previous
## one later. No need for bounds since there is no appreciable speed
## gain by specifying it.

agg.price.levels.temp <- function(x, kind){

  ## Just takes the data frame, not the entire object as input.

    stopifnot(kind %in% c("orders", "shares"))
    stopifnot(all(c("price", "size", "type") %in% names(x)))
    stopifnot(nrow(x) > 0)

    mid <- (max(x$price[x$type == "BID"]) + min(x$price[x$type ==
                           "ASK"]))/2
    
    ## Aggregate by price level. Check for NA? Must be a cooler way of
    ## doing this.

    if(kind %in% "shares"){
        x <- aggregate(x[["size"]], by = list(x[["price"]]), sum)
        names(x) <- c("price", "shares")
    } else {
        x <- aggregate(x[["size"]], by = list(x[["price"]]), length)
        names(x) <- c("price", "orders")
    }

    ## Rows with price above midpoint are ask, price below midpoint
    ## are bid.

    x$type[x[["price"]] > mid] <- "ASK"
    x$type[x[["price"]] < mid] <- "BID"

    ## Sort for ease of use and clean up row names.

    x <- x[order(x$price, decreasing = TRUE),]

    row.names(x) <- NULL

    return(x)
}
