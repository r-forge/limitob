## $Id: helper.functions.R 1300 2008-08-27 21:01:11Z liu $
##
## Internal helper functions

## This formats text for us. Type "p" means that its a price, adds
## commas and has two decimal digits. "s" means that its a size, adds
## commas and no decimal digits.

.prettify <- function(x, type = "p"){
    if(type == "p"){

        x <- formatC(x, format = "f", big.mark = ",", digits = 2)
        invisible(x)

    } else if(type == "s"){

        x <- formatC(x, format = "d", big.mark = ",")
        invisible(x)
    }
}


## Helper function that returns a data frame with the size aggregated
## by price level and with data above 10% on either side of the
## midpoint removed.  Takes an orderbook object as input. Returns
## orderbook object with price, size, and type. Mainly needed for
## plotting.

.combine.size <- function(object, bounds){

    x <- object@current.ob

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


## Returns the row number of the first order after the specified time.

.get.time.row <- function(file, n){

    invisible(.Call("retrieveTimeRow", as.character(file), as.integer(n)))
}

## Returns the time of a row number

.get.row.time <- function(file, n){

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

## .read.orders.c generates the orderbook at a specified number of
## messages.

.read.orders.c <- function(ob, n){
    x <- .Call("readOrders", as.character(ob@file), as.integer(n))


    ## Set indices for current location in the orderbook.

    ob@current.ob <- .update(x)
    ob@file.index <- n
    ob@current.time <- max(object@current.ob$time)
    invisible(ob)

}

## read.orders.multiple does what the above does, but returns a list
## with the orderbook at each row number specified

.read.orders.multiple <- function(ob, n){
    x <- .Call("readOrdersMultiple", as.character(ob@file), as.integer(n))
    x <- lapply(x, .update)

    invisible(x)

}

## Takes the a vector that is the output of .Call "readOrders" and
## turns it into a data frame.

.update <- function(x){

    ## Remove new line indicators

    x[x == "TRUE\n"] = "TRUE"
    x[x == "FALSE\n"] = "FALSE"

    len <- length(x)

    ## Create vectors for price, size, type, time, id, and the mine
    ## indicator by sequentially extracting every sixth element.

    price <- as.numeric(x[seq(4, len, 6)])
    size <- as.numeric(x[seq(5, len, 6)])
    type <- as.factor(x[seq(3, len, 6)])
    time <- as.numeric(x[seq(1, len, 6)])
    id <- as.character(x[seq(2, len, 6)])
    mine <- as.logical(x[seq(6, len, 6)])

    ## Create a dataframe containg all vectors above.

    x <- data.frame(price, size, type, time, id, mine,
                    stringsAsFactors = FALSE)

    names(x) <- c("price", "size", "type", "time", "id", "mine")

    invisible(x)

}

## Takes in object and number of lines of the data file to be
## read. Returns an object with updated current.ob,
## trade.data, my.trades, file.index, and current.time. Might remove.

.read.orders <- function(ob, n)
{
    ## Pull out current values

    file <- ob@file

    ## Open file connection. Skip to the current place in the file and
    ## read in the first line after that.

    file <- file(file, open = "r")

    x <- scan(file, nline = 1, sep = ",", what = "character", quiet =
              TRUE, skip = ob@file.index)

    ## While there are still lines to read and less than n lines have
    ## been read.

    i <- 0

    while(!identical(length(x), 0) & i < n){

        ## Increase i

        i <- i + 1

        ## If there is an add change current position, add something
        ## into ID, and increment current position.

        if (isTRUE(x[1] %in% "A")){

            ob <- add.order(ob, as.numeric(x[4]), as.numeric(x[5]),
                            as.factor(x[6]), as.numeric(x[2]),
                            as.character(x[3]), as.logical(x[7]))

        }

        ## For a cancel remove the row from remove the ID
        ## from list.

        if (isTRUE(x[1] %in% "C")){
            ob <- remove.order(ob, as.character(x[3]))
        }

        ## For a replace find the right row and replace it with the
        ## new size.

        if (isTRUE(x[1] %in% "R")){
            ob <- replace.order(ob, as.character(x[3]), as.numeric(x[4]))
        }

        ## Read in the next line.

        x <- scan(file, nline = 1, sep = ",", what = "character",
                  quiet = TRUE)

    }

    close(file)
    ob@current.time <- as.numeric(x[2])
    ob@file.index <- ob@file.index + i

    invisible(ob)
}

## Converts x to a time. x should be milliseconds since midnight
## UTC. Returns as "H:M:S".

.to.time <- function(x){
    x <- as.POSIXct(x/1000, origin = "1970-1-1")

    return(format(x, format = "%H:%M:%S"))

}

## Converts x to milliseconds. x should be a string, e.g. "5:01:02"
## means 5AM, 1 minute, 2 seconds.

.to.ms <- function(x){

    x <- strsplit(x, split = ":")[[1]]
    x <- ((as.numeric(x[1])) * 3600000
          + as.numeric(x[2]) * 60000
          + as.numeric(x[3]) * 1000)

    return(signif(x, 8))

}

.animate.seconds <- function(object, time, bounds){

    ## Create a list that will store the current orderbooks for each
    ## time, as well as the variables that hold the y and x
    ## limits. sub is for the subtitles.

    rows <- .get.time.row(object@file, time)

    time <- .to.time(time)
    current.ob = .read.orders.multiple(object, rows)
    y.limits = c(Inf, 0)
    max.size = 0

    ## Use a for loop to create all the current.ob and take the
    ## smallest/biggest axes.

    for(i in 1:length(time)){

        ## Generate the object for the next time, put the current.ob
        ## into our list, and put "" in the subtitle (no subtitles
        ## until slow).

        object@current.ob <- current.ob[[i]]

        x <- .combine.size(object, 1)
        mid <- mid.point(object)

        ## Find the min ask and max bid price for this current.ob

        ask <- x[x[["type"]] == "ASK",]
        ask <- ask[ask$price < min(ask$price) + bounds,]

        bid <- x[x[["type"]] == "BID",]
        bid <- bid[bid$price > max(bid$price) - bounds,]

        ## Check to see if the y limits are bigger/smaller than the
        ## existing ones.

        if(min(bid$price) < y.limits[1])
            y.limits[1] <- min(bid$price)

        if(max(ask$price) > y.limits[2])
            y.limits[2] <- max(ask$price)

        ## Find the max size for this current.ob

        tmp.max.size <- max(x$size[x$price <= y.limits[2] & x$price >=
                                   y.limits[1]])

        ## Check to see if the x limits are bigger/smaller than the
        ## existing ones.

        if(tmp.max.size > max.size)
            max.size <- tmp.max.size

    }

    ## Creating the x limits and tick locations

    x.at <- pretty(c(0, max.size))
    x.limits <- list(c(x.at[length(x.at)], 0),
                     c(0, x.at[length(x.at)]))

    ## Use a for-loop to create all the Trellis objects. Create a name
    ## vector.

    name = vector()

    for (i in 1:length(current.ob)){

        tmp.plot <- .animate.plot(current.ob[[i]], x.at, x.limits,
                                  y.limits, time[i])

        name[i] <- paste("y", i, sep = ".")
        assign(paste("y", i, sep = "."), tmp.plot)

    }

    ## Save the names vector

    name[length(name) + 1] = "name"
    assign("name", name)


    ## Save the Trellis objects.

    tempfile <- tempfile()
    otherfile <- object@animation[["msg"]]

    object@animation <- list(sec = tempfile, msg = otherfile)

    save(list = name, file = tempfile)

    invisible(object)

}

.animate.orders <- function(object, n, bounds){

    ## Create a list that will store the current orderbooks for each
    ## time, as well as the variables that hold the y and x
    ## limits. sub is for the subtitles.

    current.ob <- .read.orders.multiple(object, as.integer(n))

    time <- vector()
    y.limits <- c(Inf, 0)
    max.size <- 0

    ## Use a for loop to create all the current.ob and take the
    ## smallest/biggest axes.

    for(i in 1:length(n)){

        object@current.ob <- current.ob[[i]]

        time[i] <- .to.time(max(current.ob[[i]]$time))

        x <- .combine.size(object, 1)
        mid <- mid.point(object)

        ## Find the min ask and max bid price for this current.ob

        ask <- x[x[["type"]] == "ASK",]
        ask <- ask[ask$price < min(ask$price) + bounds,]

        bid <- x[x[["type"]] == "BID",]
        bid <- bid[bid$price > max(bid$price) - bounds,]

        ## Check to see if the y limits are bigger/smaller than the
        ## existing ones.

        if(min(bid$price) < y.limits[1])
            y.limits[1] <- min(bid$price)

        if(max(ask$price) > y.limits[2])
            y.limits[2] <- max(ask$price)

        ## Find the max size for this current.ob

        tmp.max.size <- max(x$size[x$price < y.limits[2] & x$price >
                                   y.limits[1]])

        ## Check to see if the x limits are bigger/smaller than the
        ## existing ones.

        if(tmp.max.size > max.size)
            max.size <- tmp.max.size

    }

    ## Creating the x limits and tick locations

    x.at <- pretty(c(0, max.size))
    x.limits <- list(c(x.at[length(x.at)], 0),
                     c(0, x.at[length(x.at)]))

    ## Use a for-loop to create all the Trellis objects and create
    ## name vector.

    name = vector()

    for (i in 1:length(current.ob)){

        tmp.plot <- .animate.plot(current.ob[[i]], x.at, x.limits,
                                  y.limits, time[i])

        name[i] <- paste("y", i, sep = ".")
        assign(paste("y", i, sep = "."), tmp.plot)

    }

    ## Save the names vector

    name[length(name) + 1] = "name"
    assign("name", name)


    ## Save the Trellis objects.

    tempfile <- tempfile()
    otherfile <- object@animation[["sec"]]

    object@animation <- list(sec = otherfile, msg = tempfile)

    save(list = name, file = tempfile)

    invisible(object)

}

## Midpoint Return, automatically finds the midpoint return for the
## selected message row number for a vector of time in seconds,
## e.g. c(5, 10, 60, 120) means find the midpoint return for 5s, 10s,
## 1 min, 2 min after the trade.

.midpoint.return <- function(object, row, time){


    ## Now the orderbook is at the start order

    object <- read.orders(object, row - object@file.index)
    startmidpt <- mid.point(object)

    ## Time is in seconds so multiply it to find milliseconds

    time <- time * 1000

    ## Pull out current time and add it to time

    time <- object@current.time + time

    midpoints <- vector()

    for(i in 1:length(time)){

        ## Pull out current row the object is at in the data file

        currentrow <- object@file.index

        ## Find the next time

        row <- .get.time.row(object@file, time[i], currentrow)

        ## Read to that time and then save the midpoint

        object <- read.orders(object, row - currentrow)
        midpoints[i] <- mid.point(object)
    }

    return(round(midpoints - startmidpt, 3))
}

## Trade weighted average price for the vector of times given the
## order number and a vector of times (like above).

.twap.return <- function(object, row, time){

    trade.data <- object@trade.data
    trdtime <- trade.data[trade.data$row == row,][[2]]
    trdprice <- trade.data[trade.data$row == row,][[3]]

    ## Time is in seconds so multiply it to find milliseconds

    time <- time * 1000

    ## Pull out current time and add it to time

    time <- trdtime + time

    twap <- vector()

    for(i in 1:length(time)){

        temp <- trade.data[trade.data$time >= trdtime &
                           trade.data$time <= time[i],]

        twap[i] = sum(temp$price * temp$size)/sum(temp$size)

    }

    return(round(twap - trdprice, 3))
}
