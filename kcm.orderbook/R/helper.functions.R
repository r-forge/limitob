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
    ## ask = x[x[[ob.names[3]]] == ob.names[6],]
    ## bid = x[x[[ob.names[3]]] == ob.names[7],]

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

.update <- function(ob)

{

    x <- ob@ob.data
    ob.names <- ob@ob.names

    ## Turn hash into a list. Unlist into a vector. Remove names.

    x = as.list(x)
    x = unlist(x, use.names = FALSE)


    ## Get out length, use it to create data frame.

    len = length(x)



    price = as.numeric(x[seq(3, len, 5)])
    size  = as.numeric(x[seq(4, len, 5)])
    type  = as.factor(x[seq(5, len, 5)])
    time  = as.numeric(x[seq(1, len, 5)])
    id    = as.character(x[seq(2, len, 5)])


    x = data.frame(price, size, type, time, id, stringsAsFactors = FALSE)

    names(x) = ob.names[1:5]

    ob@current.ob <- x
    ob@current.time <- max(x[[ob.names[4]]])
    invisible(ob)

}



## Returns the row number of the first order after the specified time.

.get.time.row <- function(feed, n, skip = 1){

    feed <- file(feed, open="r")


    x <- scan(feed, nline = 1, sep = ",", what = "character",
              quiet = TRUE, skip = skip)

    i = skip + 1

    while(length(x) != 0 & as.numeric(x[2]) <= n){

       	i = i + 1
        x <- scan(feed, nline = 1, sep = ",", what = "character", quiet = TRUE)
    }

    close(feed)

    return(i)
}

## Returns the row number of the next trade after the current time.

.get.next.trade <- function(feed, n){

    feed <- file(feed, open="r")


    x <- scan(feed, nline = 1, sep = ",", what = "character",
              quiet = TRUE, skip = n)

    n = n + 1

    while(!isTRUE(x[1] %in% "T")){
       	n = n + 1
        x <- scan(feed, nline = 1, sep = ",", what = "character", quiet = TRUE)
    }

    close(feed)

    return(n)
}


## Takes in object and number of lines to be read, 0 means read to end.

.read.orders <- function(ob, n)
{

    feed = ob@feed
    feed.index = ob@feed.index

    ob.data = ob@ob.data

    trade.data = ob@trade.data
    trade.index = ob@trade.index

    feed <- file(feed, open="r")


    x <- scan(feed, nline = 1, sep = ",", what = "character",
              quiet = TRUE, skip = feed.index)


    ## While there are still lines to read and less than n lines have been read.

    i = 1

    while(!identical(length(x), 0) & i <= n){

        ## If there is an add change current position, add something into ID, and
        ## increment current position.

        if (isTRUE(x[1] %in% "A")){

            ob.data[x[3]] <- x[2:6]

        }

        ## For a cancel remove the row from ob.data, remove the ID from list.

        if (isTRUE(x[1] %in% "C")){
            ob.data[x[3]] = NULL
        }

        ## For a replace find the right row and replace it with the new size.

        if (isTRUE(x[1] %in% "R")){
            ob.data[[x[3]]][4] = x[4]
        }

        ## For a trade increment the trade index and store the trade data.

        if (isTRUE(x[1] %in% "T")){
            trade.data[as.character(feed.index)] = x
            trade.index = trade.index + 1
        }

        ## Increase the feed index to keep track of which line we are on.

        feed.index = feed.index + 1
        i = i + 1
        x <- scan(feed, nline = 1, sep = ",", what = "character", quiet = TRUE)
    }

    close(feed)

    ob@ob.data <- ob.data
    ob@feed.index <- feed.index
    ob@trade.data <- trade.data
    ob@trade.index <- trade.index

    ob = .update(ob)

    invisible(ob)
}

## Make sure conversions are correct, need to convert from EDT to UTC for .to.ms. Need
## to convert from UTC to EDT for .to.time.

## Converts x to a time. x should be milliseconds since midnight UTC. Returns as
## "H:M:S".

.to.time <- function(x){
    x = as.POSIXct(x/1000+14400, origin = Sys.Date())

    return(format(x, format = "%H:%M:%S"))

}

## Converts x to milliseconds. x should be a string, e.g. "5:01:02" means
## 5AM, 1 minute, 2 seconds.

.to.ms <- function(x){

    x = strsplit(x, split = ":")[[1]]
    x = ((as.numeric(x[1])) * 3600000
         + as.numeric(x[2]) * 60000
         + as.numeric(x[3]) * 1000)

    return(signif(x, 8))

}

## "from" and "to" are strings in the form "%H:%M:%S", for usage of "by" see
## seq.POSIXt.

.animate <- function(object, from, to, by){

    ob.names = object@ob.names

    ## Create the vector of times

    from = as.POSIXlt(from, format = "%H:%M:%S")
    to = as.POSIXlt(to, format = "%H:%M:%S")
    time = seq.POSIXt(from, to, by)
    time = format(time, format ="%H:%M:%S")


    ## Set default settings for the plot.
    x = read.time(object, time[1])
    x = .combine.size(x, bounds = 0.1)

    ## Maximum size, max/min price and difference between the max
    ## and min price for purposes of drawing the axes.

    max.size = max(x[[ob.names[2]]])
    max.size = ceiling(max.size + max.size/20)

    min.price = signif(min(x[ob.names[[1]]])-.05,3)
    max.price = round(max(x[ob.names[[1]]])+0.5)
    midpoint = mid.point(object)

    ## Creating the x axis values.

    x.limits =  list(c(max.size,0), c(0,max.size))
    x.at = ceiling(seq(0, max.size, max.size/5))

    ## Creating the y axis values.

    tmp.at = formatC(seq(min.price, max.price, .1), format = "f", digits = 2)
    yask.at = vector()
    ybid.at = vector()

    for(i in 1:length(tmp.at)){
  	if(i%%2==0){
            yask.at[i] = tmp.at[i]
            ybid.at[i]=""
  	} else {
            yask.at[i] = ""
	   	ybid.at[i] = tmp.at[i]
   	}
    }

    new.yscale.components <- function(...) {
        ans <- yscale.components.default(...)
        ans$right <- ans$left
        ans$left$labels$labels <- ybid.at
        ans$right$labels$labels <- yask.at
        ans
    }

    ## Create the trellis objects and put them in a vector.


    for (i in 1:length(time)){
        tmp = read.time(object, time[i])
        x = .combine.size(tmp, bounds = 0.1)
        x[[ob.names[3]]] <- ordered(x[[ob.names[3]]], levels = c(ob.names[7],
                                                      ob.names[6]))

        x <- xyplot(x[[ob.names[1]]]~x[[ob.names[2]]]|x[[ob.names[3]]], data = x,
                                    ylab = "Price", xlab = "Size (Shares)", main = "Order Book",
                                    scales = list(x = list(relation = "free",
                                                  limits = x.limits,
                                                  at = x.at,
                                                  axs = "i"),
                                    y = list(at = tmp.at, alternating = 3)),
                                    yscale.components = new.yscale.components,
                                    panel = function(...){
                                        panel.xyplot(...)
                                        panel.lines(..., type = "H")
                                    }
                                    )
        assign(paste("x", i, sep = "."), x)
        object = tmp
    }

    ## "Animates" using a for loop.

    for(i in 1:length(time)){
        x = paste("x", i, sep = ".")
        print(get(x))
        Sys.sleep(1)
        rm(x)
    }

}



