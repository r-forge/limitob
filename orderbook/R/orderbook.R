## Key parts of this file are the definition of the orderbook class
## and the initialization (right word?) function. I think that
## everything else should go elsewhere.

setClass("orderbook", representation(current.ob   = "data.frame",
                                     current.time = "numeric",
                                     file         = "character",
                                     file.index   = "numeric",
                                     trade.data   = "data.frame",
                                     my.trades    = "data.frame",
                                     animation    = "list",
                                     trader       = "logical",
                                     trade.index  = "numeric"
                                     ),

         ## Orderbook class. Shouldn't we have more documentation.

         prototype(current.ob   = data.frame(),
                   current.time = 0, # Use first time in file.
                   file		= character(),
                   file.index   = 0,
                   trade.data   = data.frame(),
                   my.trades    = data.frame(),
                   animation    = list(sec = character(), msg =
                   character()),
                   trader       = FALSE,
                   trade.index  = 1
                   )
         )

## The file is a sequential list of all the orders and trades
## (messages) placed for one stock on a given day.  In order to
## recreate the order book at a given time, all messages up to that
## point are incorporated.

orderbook <- function(file, trader = TRUE) {

    ## orderbook.function.R: Returns an object of class orderbook. How
    ## should this be structured? Don't like the trader arg
    ## much. Ought to determine trader status by examining he file.


    current.ob <- data.frame(price = numeric(0), size =
                             numeric(0), type = character(0), time
                             = numeric(0), id = character(0))

    ## Check to see that the file is valid and can be opened

    obfile <- file(file, open = "r")
    stopifnot(isOpen(obfile, "r"))
    close(obfile)

    ## Look through the input file and return a vector of trade
    ## data. .Call calls C routines. The vector consists looks like
    ## c(row, time, price, size, mine, row, time, price, size,
    ## mine...).

                                        # Ought to stopifnot for file
                                        # rather than coerce.

    trade.data <- .Call("getTrades", as.character(file))

    ## Find the length of the vector.

    len <- length(trade.data)

    ## Vector is row, time, price, size, mine repeating over and over
    ## again. These are all numbers so we cast as.numeric. We pull
    ## them out from the trade data using sequence.

    row <- as.numeric(trade.data[seq(1, len, 5)]) #No idea.
    time <- as.numeric(trade.data[seq(2, len, 5)])
    price <- as.numeric(trade.data[seq(3, len, 5)])
    size <- as.numeric(trade.data[seq(4, len, 5)])

    ## If trader flag is true that means the user wants to be able to
    ## distinguish his or trades from everybody elses. So we load that
    ## data in.

                                        # Dislike this structure.

    if(isTRUE(trader)){

        ## Mine is a logical indicating whether or not the trade
        ## belongs to the trader.

        mine <- trade.data[seq(5, len, 5)]

        ## Since this is the last entry of the line it has a newline,
        ## and here we get rid of it. We don't directly rename to true
        ## or false, because there can only be one type,
        ## e.g. character or logical per column

                                        # Awkward! Must be better
                                        # way. Perhaps change the
                                        # input format so that the row
                                        # has either "1" or nothing.

        mine[mine == "FALSE\n"] <- "FALSE"
        mine[mine == "TRUE\n"] <- "TRUE"

        ## Cast as logical.

        mine <- as.logical(mine)

        ## Create trade.data data frame and then name it. Awkward!

        trade.data <- data.frame(row, time, price, size, mine)
        names(trade.data) <- c("row", "time", "price", "size", "mine")

        ## Create my.trades data frame by pulling out all rows where
        ## mine == TRUE. Reset the rownames to 1,2,3,4,5....

        my.trades <- trade.data[trade.data$mine == TRUE,]
        rownames(my.trades) <- NULL

    } else{

        ## If trader is false then just create the trade.data data
        ## frame without mine.

        trade.data <- data.frame(row, time, price, size)
        names(trade.data) <- c("row", "time", "price", "size")

        ## Make mytrades the same as trade.data to ensure other useful
        ## functions also work.

        my.trades = trade.data

    }

    ## Create a new orderbook object and return it.

    invisible(new("orderbook",
                  current.ob   = current.ob,
                  current.time = 0, # Should be first time in file.
                  trade.data   = trade.data,
                  my.trades    = my.trades,
                  file         = file,
                  trader       = trader
                  ))
}




setMethod("sort",
          signature(x = "orderbook"),
          function(x, by, decreasing = TRUE){

              ## Should use by.var. Check by.var in x@my.trades. Need
              ## this function? As a method?

              ## Sort my.trades by a user specified column and resets the
              ## trade.index to 1.

              tmp <- x@my.trades

              x@my.trades <- tmp[order(tmp[[by]], decreasing =
                                       decreasing),]

              x@trade.index <- 1

              invisible(x)
          }
          )




setMethod("[",
          signature(x = "orderbook", i = "character"),
          function(x, i){

              ## Accesses the orders at the specified price
              ## level. Does this pass R CMD check?

              ## Extract the current order book and cast i as a
              ## number.

              current.ob <- x@current.ob
              i <- as.numeric(i)

              ## Pull out all rows where the price is equal to i and
              ## erase the rownames.

              tmp <- current.ob[current.ob$price == i,]
              tmp <- tmp[order(tmp$time),]

              rownames(tmp) <- NULL

              ## Return tmp.

              return(tmp)
          }
          )


