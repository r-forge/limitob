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

    ## Create a data frame out of the matrix, we cast during the creation and
    ## then use ob.names to assign names.

    x = data.frame(as.numeric(x[,1]), as.numeric(x[,2]), as.factor(x[,3]),
    as.numeric(x[,4]), as.character(x[,5]))

    names(x) = ob.names[1:5]

    ## Remove anything with NA in the price column, because thats how we cancel orders.

    current.ob = x[!is.na(x[1]),]


    ## Reupdate the list of ids if there are IDs in the orderbook. Also update
    ## the current time if there are orders in the book.

    if(nrow(current.ob) > 1){
        ids = hash()

        for(i in 1:nrow(current.ob)){
            ids[current.ob[i,][[5]]] = i
        }

        ob@current.time <- max(current.ob[[ob.names[4]]])
    } else {
        ids = hash()

        ob@current.time <- 0
    }

    ## Reupdate ob.data.

    ob.data = matrix(ncol = 5, nrow = n)

    colnames(ob.data) = names(current.ob)

    ob.data = rbind(as.matrix(current.ob), ob.data)

    ## Set new variables then return.

    ob@current.ob <- current.ob
    ob@ob.data <- ob.data
    ob@current.pos <- nrow(current.ob) + 1
    ob@ids <- ids

    invisible(ob)

}

## Takes in object and number of lines to be read, 0 means read to end.

.read.orders <- function(ob, n)
{

    ob = .update(ob, n)


    feed = ob@feed
    feed.index = ob@feed.index

    ob.data = ob@ob.data
    current.pos = ob@current.pos

    ids = ob@ids

    trade.data = ob@trade.data

    feed <- file(feed, open="r")


    x <- scan(feed, nline = 1, sep = ",", what = "character",
              quiet = TRUE, skip = feed.index)


    ## While there are still lines to read and less than n lines have been read.

    i = 1

    while(length(x) != 0 & i <= n){

        ## If there is an add change current position, add something into ID, and
        ## increment current position.

        if (x[1]=="A"){
            ob.data[current.pos,] = c(x[4], x[5], x[6], x[2], x[3])
            ids[x[3]] = current.pos
            current.pos = current.pos + 1
        }

        ## For a cancel remove the row from ob.data, remove the ID from list.

        if (x[1]=="C"){
            ob.data[ids[[x[3]]],][1] = NA
            ids[x[3]] = NULL
        }

        ## For a replace find the right row and replace it with the new size.

        if (x[1]=="R"){
            ob.data[ids[[x[3]]],][2] = x[4]
        }

        ## For a trade increment the trade index and store the trade data.

        if (x[1] == "T"){
            trade.data[as.character(feed.index)] = list(x)
        }

        ## Increase the feed index to keep track of which line we are on.

        feed.index = feed.index + 1
        i = i + 1
        x <- scan(feed, nline = 1, sep = ",", what = "character", quiet = TRUE)
    }

    ob@ob.data <- ob.data
    ob@feed.index <- feed.index
    ob@trade.data <- trade.data


    close(feed)

    ob = .update(ob, 0)

    invisible(ob)
}
