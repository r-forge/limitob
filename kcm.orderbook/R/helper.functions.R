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

.combine.size <- function(object, bounds){


    ## Pull out the current.ob and ob.names.

    x = object@current.ob
    ob.names = object@ob.names


    ## Removes rows 10% above and below the midpoint.

    x = x[x[[ob.names[1]]] < mid.point(object)*(1 + bounds) &
    x[[ob.names[1]]] > mid.point(object)*(1 - bounds),]

    ## Splits x into ask and bid data frames.
	x <- split(x, x[[ob.names[3]]])
	ask <- x[[ob.names[6]]]
	bid <- x[[ob.names[7]]]
    #ask = x[x[[ob.names[3]]] == ob.names[6],]
	#bid = x[x[[ob.names[3]]] == ob.names[7],]

    ## Aggregate sizes by price level.

	if(nrow(ask) > 0){
            ask = aggregate(ask[[ob.names[2]]], by = list(ask[[ob.names[1]]]), sum)
            ask <- data.frame(ask, type = rep(ob.names[6], nrow(ask)))
	}

	if(nrow(bid) > 0){
            bid = aggregate(bid[[ob.names[2]]], by = list(bid[[ob.names[1]]]), sum)
            bid <- data.frame(bid, type = rep(ob.names[7], nrow(bid)))
	}

    x  = rbind(ask, bid)

    ## Adds a type column to x.

    #x = data.frame(x, type = character(nrow(x)))
    ##levels(x$type) = c(ob.names[6], ob.names[7])

    ## Fills in the type column depending on if the price levels are in the ask
    ## or bid data frames.

    ##x$type[x[[1]] %in% ask[[ob.names[1]]]] = ob.names[6]
    ##x$type[x[[1]] %in% bid[[ob.names[1]]]] = ob.names[7]

    ## Names the new data frame and returns it.
    names(x) = c(ob.names[1], ob.names[2], ob.names[3])

    return(x)
}

## Creates a new current.ob from the ob.data.

.update <- function(ob, n)

{
    x <- ob@ob.data
    ob.names <- ob@ob.names

    ## Remove anything with NA in the price column, because thats how we cancel orders.

    current.ob = x[!is.na(x[1]),]


    ## Casting everything to make sure it is the desired data type. For some
    ## reason ID doesn't really get casted so I'll cast it again later.

    current.ob[1] = as.numeric(current.ob[,1])
    current.ob[2] = as.numeric(current.ob[,2])
    current.ob[3] = as.factor(current.ob[,3])
    current.ob[4] = as.numeric(current.ob[,4])
    current.ob[5] = as.character(current.ob[,5])

    ## Reupdate the list of ids.

    ids = hash()
    for(i in 1:nrow(current.ob)){
        ids[current.ob[i,][[5]]] = i
    }

    ## Reupdate ob.data.

    ob.data = data.frame(rep(NA, n),rep(NA, n),rep(NA, n),
    rep(NA, n),rep(NA, n))

    names(ob.data) = names(current.ob)

    ob.data = rbind(current.ob, ob.data)

    ## Set new variables then return.

    ob@current.ob <- current.ob
    ob@current.time <- max(current.ob[[ob.names[4]]])
    ob@ob.data <- ob.data
    ob@current.pos <- nrow(current.ob) + 1
    ob@ids <- ids

    invisible(ob)

}
