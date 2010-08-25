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
                                        # NA.x

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

setMethod("show",
          signature(object = "orderbook"),
          function(object){

              ## Show basic information about the order book at its current
              ## state. .prettify is a helper function to make the output print
              ## nicely.

                                        # Deserves own file.

              cat("An object of class orderbook\n")
              cat("--------------------------\n")
              cat("Current orderbook time:   ",
                  .to.time(object@current.time), "\n")
              cat("Message Index:            ",
                  .prettify(object@file.index, "s"), "\n")
              cat("Bid Orders:               ",
                  .prettify(bid.orders(object), "s"), "\n")
              cat("Ask Orders:               ",
                  .prettify(ask.orders(object), "s"), "\n")
              cat("Total Orders:             ",
                  .prettify(total.orders(object), "s"), "\n")
          }
          )

                                        # Put all the plotting stuff together.

setMethod("plot",
          signature(x = "orderbook"),
          function(x, bounds = 0.1, n = 10, type = "n"){

              ## Plot calls helper plot methods. See orderbook.plot.R for more
              ## details.

              ## Check the type of plot the user wants then calls the
              ## helper function. Most plot helper functions return a
              ## Trellis object that we need to print.

              if(isTRUE(type %in% "n")){

                  ## Plots the size of the orders at each bid and ask
                  ## independently.

                  tmp <- .plot.ob(x, bounds)
                  print(tmp)

              } else if(isTRUE(type %in% "s")){

                  ## Plots the size of each level of bid and ask
                  ## together. ie. best bid and best ask are the same
                  ## level.

                  .plot.side.ob(x, n)

              } else if(isTRUE(type %in% "o")){

                  ## Plots the number of orders at each bid and ask.

                  tmp <- .plot.orders.ob(x, bounds)
                  print(tmp)

              } else if(isTRUE(type %in% "sd")){

                  ## Graphs the normalized price against normalized
                  ## size to reflect a supply/demand curve.

                  tmp <- .supply.demand.plot(x, bounds)
                  print(tmp)

              } else {

                  print("Invalid type")

              }
          }
          )

                                        # Separate file.
setMethod("summary",
          signature(object = "orderbook"),
          function(object){

              ## Displays summary information.

              cat("\nCurrent time is",
                  .to.time(object@current.time), "\n\n")
              cat("Ask price levels:  ",
                  .prettify(ask.price.levels(object), "s"), "\n")
              cat("Bid price levels:  ",
                  .prettify(bid.price.levels(object), "s"), "\n")
              cat("Total price levels:",
                  .prettify(total.price.levels(object), "s"), "\n")
              cat("-----------------------------\n")
              cat("Ask orders:        ",
                  .prettify(ask.orders(object), "s"), "\n")
              cat("Bid orders:        ",
                  .prettify(bid.orders(object), "s"), "\n")
              cat("Total orders:      ",
                  .prettify(total.orders(object), "s"), "\n")
              cat("-----------------------------\n")
              cat("Spread:            ",
                  .prettify(spread(object)), "\n\n")

              ## Retrieve the midpoint.

              mid <- mid.point(object)[[1]]

              ## Subtract floor(mid) from midpoint. This basically
              ## just gets the decimal portion.

              check <- mid - floor(mid)

              ## If it has less than 3 decimal places (5
              ## characters--0.123) format so 2 decimal places are
              ## displayed, otherwise do nothing.

              if(nchar(check) < 5)
                 mid <- formatC(mid, format = "f", digits = 2)

              cat("Mid point:         ", mid, "\n")
              cat("-----------------------------\n")
              cat("Inside market \n \n")

              ## Calls inside.market function to print the inside
              ## market.

              inside.market(object)   # Hmmmm. Does this (stupidly)
                                      # drive inside.market() design?
              cat("\n")
          }
          )

                                        # Separate file.
setMethod("display",
          signature(object = "orderbook"),
          function(object, n = 5, ...){

              ## Displays the price levels and sizes. n specifies the number of rows to be
              ## displayed for ask and bid.

              ## Combine size returns a data frame with the size
              ## aggregated for each price level. Output data frame is
              ## sorted.

              x <- .combine.size(object, Inf)

              ## Create ask and bid data frames

              ask <- x[x[["type"]] == "ASK",]
              bid <- x[x[["type"]] == "BID",]

              ## Print out current time.

              cat("\nCurrent time is",
                  .to.time(object@current.time), "\n\n")
              cat("\t\t Price \t Ask Size\n")
              cat("---------------------------------------------\n")

              ## Print out the top n ask prices/sizes

              for(i in rev(1:min(n, nrow(ask)))){
                  cat("\t\t",
                      .prettify(ask[["price"]][i]), "\t",
                      .prettify(ask[["size"]][i], "s"), "\n")
              }
              cat("---------------------------------------------\n")

              ## Print out the top n bid prices/sizes

              for(i in rev(max(1, nrow(bid) - n + 1):nrow(bid))){

                  ## Spacing for right alignment, max size is
                  ## 1 million for this to work

                  size = .prettify(bid[["size"]][i], "s")

                  ## If its less than 7 characters, then add spaces
                  ## before you print.

                  if(nchar(size) <= 7){
                      space = rep(" ", 7 - nchar(size))
                  } else {
                      space = ""
                  }

                  ## Combine the spaces into one big space string.

                  space = paste(space, collapse = "")

                  ## Combine space with the size.

                  size = paste(space, size, sep = "")

                  ## Actually printing it out

                  cat(size, "\t",
                      .prettify(bid[["price"]][i]), "\n")
              }
              cat("---------------------------------------------\n")
              cat("Bid Size \t Price\n")

          }
          )


                                        # Single function:
                                        # ob.levels(). No need for
                                        # these to be methods, I
                                        # think.

setMethod("bid.price.levels",
          signature(object = "orderbook"),
          function(object, ...) {

              ## Returns the number of bid price levels.

              x <- object@current.ob

              ## Pull out the bids.

              x <- x[x[["type"]]=="BID",]

              ## Use table to count.

              return(length(table(x[["price"]], exclude = NA)))
          }
          )


setMethod("ask.price.levels",
          signature(object = "orderbook"),
          function(object, ...) {

              ## Returns the number of ask price levels. See above.

              x <- object@current.ob

              x <- x[x[["type"]]=="ASK",]

              return(length(table(x[["price"]], exclude = NA)))

          }
          )




setMethod("total.price.levels",
          signature(object = "orderbook"),
          function(object, ...) {

              ## Returns the total number of price levels.

              return(bid.price.levels(object) +
                     ask.price.levels(object))

          }
          )

                                        # Single function:
                                        # ob.orders(). No need for
                                        # these to be methods, I
                                        # think. Or maybe, one
                                        # function covers levels and
                                        # orders. type %in% bid, ask,
                                        # all.


setMethod("bid.orders",
          signature(object = "orderbook"),
          function(object, ...) {

              ## Returns the number of bid orders.

              x <- object@current.ob

              ## Isolate bids from order book.

              x = x[x[["type"]] == "BID",]

              ## Take care of NAs by using max. Count using nrow.

              return(max(0, nrow(x)))
          }
          )


setMethod("ask.orders",
          signature(object = "orderbook"),
          function(object, ...) {

              ## Returns the number of ask orders. See above.

              x <- object@current.ob

              x = x[x[["type"]] == "ASK",]

              return(max(0, nrow(x)))
          }
          )



setMethod("total.orders",
          signature(object = "orderbook"),
          function(object, ...) {

              ## Returns the total number of orders.

              return(ask.orders(object) + bid.orders(object))

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

                                        # Put all animation together
                                        # on separate page.

setMethod("load.animation",
          signature(object = "orderbook"),
          function(object, from, to, fps = 1, by = "sec", bounds =
                   0.02){

              ## Create Trellis objects that are used for the animation and save
              ## them using tempfile(). Put the location of tempfile() in
              ## orderbook@animation. Can view by message or seconds.

              if(isTRUE(by %in% "sec")){

                  time <- seq(.to.ms(from), .to.ms(to), 1000/fps)

                  ## Run helper function to do the actual creation of
                  ## the Trellis objects.

                  invisible(.animate.seconds(object, time, bounds))

              } else if(isTRUE(by %in% "msg")){

                  ## Run helper function to do actual creation of the
                  ## Trellis objects.

                  invisible(.animate.orders(object, seq(from, to),
                                            bounds))

              }
          }
          )


setMethod("load.trade.animation",
          signature(object = "orderbook"),
          function(object, tradenum, before = 30, after = 30, fps = 1, by =
                   "both", bounds = 0.02){

              ## Load trade animation given a trade number.

              ## Extract the desired trade from my trades.

              trade <- object@my.trades[tradenum,]

              if(isTRUE(by %in% "sec") | isTRUE(by %in% "both")){

                  ## Find the time 30 seconds before and 30 seconds
                  ## (30 seconds is the default) after that time.

                  ## Obtain the time by pulling the 2nd column.

                  tradetime <- trade[[2]]

                  ## Convert to an actual time format.

                  tradetime <- .to.time(tradetime)

                  ## Create a POSIX object.

                  tradetime <- as.POSIXlt(tradetime, format = "%H:%M:%S")

                  ## Establish the range of time to observe in the animation.

                  from <- format(tradetime - before, format = "%H:%M:%S")
                  to <- format(tradetime + after, format = "%H:%M:%S")

                  time <- seq(.to.ms(from), .to.ms(to), 1000/fps)

                  object <- .animate.seconds(object, time, bounds, trade)
              }

              if(isTRUE(by %in% "msg") | isTRUE(by %in% "both")){

                  ## Obtain the row by pulling the first column.

                  traderow <- trade[[1]]

                  ## Animate the orderbook from n to before + after messages after n.

                  object <- .animate.orders(object, seq(traderow -
                                                        before,
                                                        traderow +
                                                        after),
                                            bounds, trade)

              }

              ## Set a new trade.index and return the object.

              object@trade.index <- tradenum
              invisible(object)

          }
          )


                                        # If these are only used by
                                        # animation, then the names
                                        # are bad.

setMethod("load.next.trade",
          signature(object = "orderbook"),
          function(object, before = 30, after = 30, fps = 1, by =
                   "both", bounds = .02){

              ## Loads trade animation for the current trade index then increments
              ## it by 1.

              object <- load.trade.animation(object,
                                             object@trade.index,
                                             before, after, fps, by,
                                             bounds)

              object@trade.index <- object@trade.index + 1

              invisible(object)
          }
          )


setMethod("load.previous.trade",
          signature(object = "orderbook"),
          function(object, before = 30, after = 30, fps = 1, by =
                   "both", bounds = .02){

              ## Loads trade animation for current trade index - 1 then decrements
              ## it by 1.

              if(object@trade.index > 1){

                  object <- load.trade.animation(object,
                                                 object@trade.index - 1,
                                                 before, after, fps, by,
                                                 bounds)

                  object@trade.index <- object@trade.index - 1

              }

              invisible(object)

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


setMethod("animate",
          signature(object = "orderbook"),
          function(object, by = "sec", start = NULL, end = NULL, pause = 0.25, initPause = 2){

              ## Animate is to be used in conjunction with
              ## load.trade.animation or load.animation. After an
              ## animation is created its location is stored in
              ## object@animation. This function loads the animation
              ## and uses a for loop to play through it.

              ## Load the trade animation stored in object@animation
              ## according to type.

              filename <- object@animation[[by]]
              load(filename)

              ## Remove "name" slot from the name vector.

              name <- name[-length(name)]

              ## Initial pause

              Sys.sleep(initPause)

              ## If start is null, then make it 1, otherwise its the
              ## middle of name - start

              if(is.null(start))
                  start <- 1
              else
                  start <- ceiling(length(name)/2) - start

              ## If end is null, then make it the length of name,
              ## otherwise its the middle of name + start

              if(is.null(end))
                 end <- length(name)
              else
                 end <- floor(length(name)/2) + end

              ## Loop through name to print all the objects.

              for(i in start:end){

                  print(get(name[i]))

                  ## Pause during each plot.

                  Sys.sleep(pause)
              }

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





