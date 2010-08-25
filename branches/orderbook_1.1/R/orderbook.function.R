orderbook <- function(file, trader = TRUE) {

    ## orderbook.function.R: Returns an object of class limitob. How
    ## should this be structured? Don't like the trader arg much.



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



