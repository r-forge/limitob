## Internal helper functions. Not all of these belong here.

.prettify <- function(x, type = "p"){

    ## This formats text for us. Type "p" means that its a price, adds
    ## commas and has two decimal digits. "s" means that its a size,
    ## adds commas and no decimal digits.

    stopifnot(type %in% c("p", "s"))

    if(type == "p"){

        x <- formatC(x, format = "f", big.mark = ",", digits = 2)
        invisible(x)

    } else if(type == "s"){

        x <- formatC(x, format = "d", big.mark = ",")
        invisible(x)
    }
}


.combine.size <- function(object, bounds){

                                        #object a good name?
                                        #Functions should be visible.

    ## Helper function that returns a data frame with the size aggregated
    ## by price level and with data above 10% on either side of the
    ## midpoint removed.  Takes an orderbook object as input. Returns
    ## orderbook object with price, size, and type. Mainly needed for
    ## plotting.


    x <- object@current.ob #Hate that slot name.

                                        #Ought to check that current.ob has these vars.

    ## Save the midpoint

    mid <- mid.point(object)

    ## Removes rows 10% above and below the midpoint.

    x <- x[x[["price"]] < mid*(1 + bounds) &
           x[["price"]] > mid*(1 - bounds),]

    ## Aggregate by price

    x <- aggregate(x[["size"]], by = list(x[["price"]]), sum)

    names(x) <- c("price", "size")

    ## Rows with price above midpoint are ask, price below midpoint
    ## are bid.

    x$type[x[["price"]] > mid] <- "ASK"
    x$type[x[["price"]] < mid] <- "BID"

    return(x)
}



.get.time.row <- function(file, n){

    ## Returns the row number of the first order after the specified
    ## time. Deserves own method?

    invisible(.Call("retrieveTimeRow", as.character(file), as.integer(n)))
}


.get.row.time <- function(file, n){

    ## Returns the time of a row number

    ## Open the file connection

    file <- file(file, open = "r")

    ## Skip to 1 before the row in question, then read the line

    x <- scan(file, nline = 1, sep = ",", what = "character",
              quiet = TRUE, skip = n - 1)

    ## Close the connection

    close(file)

    ## Return the time, the second element in the vector x.

    return(as.numeric(x[2]))
}

## Why do we need these special helper functions? Could they just be
## integrated into the spot where they are called.

.read.messages.c <- function(ob, n){

    ## .read.messages.c generates the orderbook at a specified number of
    ## messages.

    x <- .Call("readOrders", as.character(ob@file), as.integer(n))

    ## Set indices for current location in the orderbook.

    ob@current.ob <- .update(x)
    ob@file.index <- n
    ob@current.time <- max(ob@current.ob$time)
    invisible(ob)

}

## Is this version necessary/useful?

.read.messages.multiple <- function(ob, n){

    ## read.messages.multiple does what the above does, but returns a
    ## list with the orderbook at each row number specified. Ought to
    ## change the name of the C code.

    x <- .Call("readOrdersMultiple", as.character(ob@file), as.integer(n))
    x <- lapply(x, .update)
    invisible(x)

}


.update <- function(x){

    ## Takes the a vector that is the output of .Call "readOrders" and
    ## turns it into a data frame.

    ## Remove new line indicators

    x[x == "TRUE\n"] = "TRUE"     #Awkward! Must be a better way.
    x[x == "FALSE\n"] = "FALSE"

    len <- length(x)

    ## Create vectors for price, size, type, time, id, and the mine
    ## indicator by sequentially extracting every sixth element.

                                        #No idea what this is doing.

    price <- as.numeric(x[seq(4, len, 6)])
    size <- as.numeric(x[seq(5, len, 6)])
    type <- as.factor(x[seq(3, len, 6)])
    time <- as.numeric(x[seq(1, len, 6)])
    id <- as.character(x[seq(2, len, 6)])
    status <- factor(x[seq(6, len, 6)], levels = c("a", "b", "c", "d",
                                        "e"))

    ## Create a dataframe containg all vectors above.

    x <- data.frame(price, size, type, time, id, status,
                    stringsAsFactors = FALSE)

    names(x) <- c("price", "size", "type", "time", "id", "status")


    invisible(x)

}


.to.time <- function(x){

    ## Converts x to a time. x should be milliseconds since midnight
    ## UTC. Returns as "H:M:S".


    x <- as.POSIXct(x/1000, origin = "1970-1-1")

    return(format(x, format = "%H:%M:%S"))

}


.to.ms <- function(x){

    ## Converts x to milliseconds. x should be a string, e.g. "5:01:02"
    ## means 5AM, 1 minute, 2 seconds.

    x <- strsplit(x, split = ":")[[1]]
    x <- ((as.numeric(x[1])) * 3600000
          + as.numeric(x[2]) * 60000
          + as.numeric(x[3]) * 1000)

    return(signif(x, 8))

}
