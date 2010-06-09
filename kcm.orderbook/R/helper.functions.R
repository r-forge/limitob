################################################################################
##
## $Id: helper.functions.R 1300 2008-08-27 21:01:11Z zhao $
##
## Internal helper functions
##
################################################################################

## This formats text for us. Type "p" means that its a price, adds commas and
## has two decimal digits. "s" means that its a size, adds commas and no
## decimal digits.

.prettify <- function(x, type = "p"){
    if(type == "p"){
        x = formatC(x, format = "f", big.mark = ",", digits = 2)
        invisible(x)
    } else if(type == "s"){
        x = formatC(x, format = "d", big.mark = ",")
        invisible(x)
    }
}


## Helper function that returns a data frame with the size aggregated by
## price level and with data above 10% on either side of the midpoint removed.
## Takes an orderbook object as input. Returns orderbook object with price,
## size, and type. Mainly needed for plotting.

.combine.size <- function(object, bounds = 0.1){

    ## Pull out the current.ob and ob.names.

    x = object@current.ob
    ob.names = object@ob.names

    ## Removes rows 10% above and below the midpoint.

    x = x[x[[ob.names[1]]] < mid.point(object)*(1 + bounds) &
    x[[ob.names[1]]] > mid.point(object)*(1 - bounds),]

    ## Splits x into ask and bid data frames.

    ask = x[x[[ob.names[3]]] == ob.names[6],]
    bid = x[x[[ob.names[3]]] == ob.names[7],]

    ## Aggregate sizes by price level.

    x  = aggregate(x[[ob.names[2]]], by = list(x[[ob.names[1]]]), sum)

    ## Adds a type column to x.

    x = data.frame(x, type = character(nrow(x)))
    levels(x$type) = c(ob.names[6], ob.names[7])

    ## Fills in the type column depending on if the price levels are in the ask
    ## or bid data frames.

    x$type[x[[1]] %in% ask[[ob.names[1]]]] = ob.names[6]
    x$type[x[[1]] %in% bid[[ob.names[1]]]] = ob.names[7]

    ## Names the new data frame and returns it.
    names(x) = c(ob.names[1], ob.names[2], ob.names[3])

    return(x)
}
