################################################################################
##
## $Id: orderbook.function.R 1300 2008-08-27 21:01:11Z zhao $
##
## Returns an object of class limitob
##
################################################################################

## Returns an orderbook object. For input it takes a data frame, and names for
## price, size, type, time, id, as well as what ASK and BID are denoted as.

orderbook <- function(x,
                      price = "price",
                      size = "size",
                      type = "type",
                      time = "time",
                      id = "id",
                      ask = "ASK",
                      bid = "BID"){

    ## Make sure the user inputted correct names for the columns, ie the
    ## columns named actually exist in data frame x.
    if(nrow(x) != 0){
        if(!(id %in% names(x) &
             price %in% names(x) &
             size %in% names(x) &
             type %in% names(x) &
             time %in% names(x))){
            stop("Wrong variable name")
        }



        ## Casting everything to make sure it is the desired data type.

        x[price] = as.numeric(x[,price])
        x[size] = as.numeric(x[,size])
        x[type] = as.factor(x[,type])
        x[time] = as.numeric(x[,time])
        x[id] = as.character(x[,id])

        ## Get rid of junk data--rows with no time information, rows that are
        ## not of type ask or bid, and rows where price/size is 0.

        x = x[!is.na(x[,time]),]
        x = x[x[type] == ask | x[type] == bid,]
        x = x[x[price] != 0,]
        x = x[x[size] != 0,]

        ## Store the current order book as current.ob, and the user inputted
        ## names as ob.names. We use ob.names throughout the rest of the
        ## package and here is where we assign the names!

        ob.names = c(price, size, type, time, id, ask, bid)
        current.ob = x[,which(names(x) %in% ob.names)]

        ## Put a time on the order book. We assume that it is the last timestamp
        ## in the order book.

        end = max(x[[time]]) + 1

        ## Return a new orderbook object.

        invisible(new("orderbook",
                      current.ob = current.ob,
                      current.time = end,
                      ob.names = ob.names
                      ))
    } else {

        ob.names = c(price, size, type, time, id, ask, bid)
        invisible(new("orderbook",
                      current.ob = data.frame(price = c(), size = c(),
                      type = c(), time = c(), id = c()),
                      current.time = 0,
                      ob.names = ob.names
                      ))
    }

}
