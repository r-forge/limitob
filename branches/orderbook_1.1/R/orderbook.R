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





                                        # Don't a lot of these methods
                                        # belong in their own files,
                                        # and with better help pages?

setMethod("read.orders",
          signature(object = "orderbook"),
          function(object, n = 1000){

              ## The following function reads the next n messages from the file from
              ## the current location within the orderbook (file.index). Use
              ## negative n to read previous messages. For example, typing
              ## read.orders(object, 100) would read 100 rows of the input file.

              stopifnot(is.numeric(n))

              if(identical(n, 0)){

                  ## If user types in 0 for n, do nothing and return
                  ## object.

                  return(object)

              } else if(n > 0){

                  ## If reading in a positive numver of orders, call
                  ## the C routine using .read.orders.c.

                  invisible(.read.orders.c(object, object@file.index +
                                           n))

              } else if(n < 0){

                  ## Then reset the object and read in to
                  ## object@file.index + n rows. Since n is negative
                  ## we can add.

                  n <- object@file.index + n
                  object <- reset(object)
                  invisible(.read.orders.c(object, n))

              }

          }
          )

setMethod("read.time",
           signature(object = "orderbook"),
           function(object, t){

               ## Reads orders from the file until the time specified. For example,
               ## read.time(object, "9:30:00") returns the order book at 9:30:00.

               ## .get.time.row will return the row number of the
               ## first message with time greater than or equal to
               ## n. .to.ms converts n to milliseconds after midnight.

               t <- .get.time.row(object@file, .to.ms(t))

               ## Reset the object.

               object <- reset(object)

               ## Use read.orders to get to row n.

               invisible(read.orders(object, t))

          }
          )



                                        # Seems horribly redundant!
                                        # Just give inside.market an
                                        # argument which allows for
                                        # "bid", "ask" or "both."

setMethod("best.bid",
          signature(object = "orderbook"),
          function(object, ...){

              ## Returns a vector with the price and size of the best bid at the
              ## current order book state.


              ## Pull out current.ob into x.

              x <- object@current.ob

              ## Takes out the bids.

              x <- x[x[["type"]] == "BID",]

              ## Return -1 if x is empty, i.e. there are no bids.

                                        # Hate this. Ought to return
                                        # NA.

              if(identical(nrow(x), 0)){

                  return(-1)

              } else {

                  ## Gets indices for the best bid.

                  index <- which(x[["price"]] == max(x[["price"]]))

                  ## Any of the indices would get us the best price, so we
                  ## just use the first one.

                  price <- x[["price"]][index[1]]

                  ## Sum to find the total size at the indices.

                  size <- sum(x[["size"]][index])

                  ## Return named vector of the best bid price and
                  ## total size at that price level.

                  return(c(price = price, size = size))
              }
          }
          )


setMethod("best.ask",
          signature(object = "orderbook"),
          function(object, ...){

              ## Returns a vector with the price and size of the best ask at the
              ## current order book state. See comments above. Code is identical
              ## except we find min instead of max price.

                                        # What a hack! Only three
                                        # letters are differennt!

                                        # Easy enough to combine these.

              x <- object@current.ob

              x <- x[x[["type"]] == "ASK",]

              if(identical(nrow(x), 0)){

                  return(-1)

              } else {

                  index <- which(x[["price"]] == min(x[["price"]]))

                  price <- x[["price"]][index[1]]
                  size <- sum(x[["size"]][index])

                  return(c(price = price, size = size))
              }

          }
          )



setMethod("mid.point",
          signature(object = "orderbook"),
          function(object, ...) {

              ## Returns the midpoint value, which is just the simple average of the
              ## best bid and ask.

              return((best.bid(object)[1] + best.ask(object)[1])/2)

          }
          )

setMethod("inside.market",
          signature(object = "orderbook"),
          function(object, invis = FALSE, ...){

              ## Returns a data frame with a row for the best ask and a row for the
              ## best bid.  The columns are price, size, and type.

              ask <- best.ask(object)
              bid <- best.bid(object)

              ## If invis is TRUE it won't print but will return the
              ## inside market object

              if(invis == FALSE){
                  cat("Best Bid:          ",
                      .prettify(bid["price"]), "\n")
                  cat("Size:              ",
                      .prettify(bid["size"], "s"), "\n \n")
                  cat("Best Ask:          ",
                      .prettify(ask["price"]), "\n")
                  cat("Size:              ",
                  .prettify(ask["size"], "s"), "\n")
              }

              ## Returns the inside market as an object

              invisible(rbind(ask, bid))

          }
          )

setMethod("spread",
          signature(object = "orderbook"),
          function(object, ...){

              ## Returns the spread.
                                        # Combine with midpoint?
                                        # Test cases?

              ## Gets best bid and best ask.

              ask = best.ask(object)
              bid = best.bid(object)

              ## If there are either no bids or no asks, then return
              ## NA. Otherwise return the difference of the bestask
              ## and best bid.

              if(ask[["price"]] == -1 | bid[["price"]] == -1){

                  return(NA)

              } else{

                  return(ask[["price"]] - bid[["price"]])

              }

          }
          )


                                        # Should use by.var. Check
                                        # by.var in x@my.trades. Need
                                        # this function? As a method?

setMethod("sort",
          signature(x = "orderbook"),
          function(x, by, decreasing = TRUE){

              ## Sort my.trades by a user specified column and resets the
              ## trade.index to 1.

              tmp <- x@my.trades

              x@my.trades <- tmp[order(tmp[[by]], decreasing =
                                       decreasing),]

              x@trade.index <- 1

              invisible(x)
          }
          )


setMethod("reset",
          signature(object = "orderbook"),
          function(object){

              ## Clears current.ob, does not clear trade data, my trades, or
              ## trade.index. Hmmmm. Necessary?

              current.ob <- data.frame(numeric(0), numeric(0),
                                       character(0), numeric(0), character(0))

              names(current.ob) <- c("price", "size", "type", "time",
                                     "id")

              object@current.time <- 0
              object@file.index <- 0
              object@current.ob <- current.ob

              invisible(object)
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

                                        # Why not just a function?

setMethod("initialize.trades",
          signature(object = "orderbook"),
          function(object, time = c(5, 300)){

              ## Initialize Trades--calculate midpoint return/trade
              ## weighted average price return for all trades in
              ## my.trades. Takes the object as well as a vector of
              ## time in seconds to decide the times for finding the
              ## returns.

              ## Get my trades.

              mytrades <- object@my.trades

              ## Create a list to store the trade returns.

              tradereturns = list()

              ## For each of my trades

              for(i in 1:nrow(mytrades)){

                  ## Find its row, price, and time within the data
                  ## file

                  trdrow <- mytrades$row[i]
                  trdprice <- mytrades$price[i]
                  trdtime <- mytrades$time[i]

                  ## Given that information find the midpoint and twap

                  midpt.ret <- .midpoint.return(object, trdprice,
                                                trdrow, trdtime, time)

                  tmp <- c(midpt.ret[[1]],
                           .twap.return(object, trdprice, trdtime,
                                        time, midpt.ret[[2]]))

                  ## Put that vector into a list

                  tradereturns[[i]] <- tmp
              }

              ## Turn the list into a matrix

              tmp <- do.call(rbind, tradereturns)

              ## Bind the returns with mytrades

              mytrades <- cbind(mytrades, tmp)

              ## Create a name vector and double the time vector (for
              ## midpoint and twap)

              names = vector()

              ## Create column names for midpoint

              for(i in 1:length(time)){
                  names[i] = paste("midpoint", time[i], sep = ".")
              }

              ## Create column names for twap

              for(i in (length(time) + 1):(length(time) * 2)){
                  names[i] = paste("twap", time[i - length(time)], sep
                       = ".")
              }

              ## Put the names on the data frame

              names(mytrades) <- append(names(mytrades)[1:5], names)
              rownames(mytrades) <- NULL

              ## Put the new data frame into my trades

              object@my.trades <- mytrades

              ## Return the object

              return(object)
          }
          )

