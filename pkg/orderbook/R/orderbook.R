## orderbook.R: functions of the orderbook object
##
## orderbook is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## limitob is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with orderbook.  If not, see <http://www.gnu.org/licenses/>.

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

         prototype(current.ob   = data.frame(),
                   current.time = 0,
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

## The following function reads the next n messages from the file from
## the current location within the orderbook (file.index). Use
## negative n to read previous messages. For example, typing
## read.orders(object, 100) would read 100 rows of the input file.

setMethod("read.orders",
          signature(object = "orderbook"),
          function(object, n = 1000){

              if(identical(n, 0)){

                  ## If user types in 0 for n, do nothing and return
                  ## object.

                  return(object)

              } else if(n > 0){

                  ## If reading in more then 5 orders, call the C
                  ## routine using .read.orders.c.

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

## Reads orders from the file until the time specified. For example,
## read.time(object, "9:30:00") returns the order book at 9:30:00.

setMethod("read.time",
           signature(object = "orderbook"),
           function(object, n){

               ## .get.time.row will return the row number of the
               ## first message with time greater than or equal to
               ## n. .to.ms converts n to milliseconds after midnight.

               n <- .get.time.row(object@file, .to.ms(n))

               ## Reset the object.

               object <- reset(object)

               ## Use read.orders to get to row n.

               invisible(read.orders(object, n))

          }
          )




## Returns a vector with the price and size of the best bid at the
## current order book state.

setMethod("best.bid",
          signature(object = "orderbook"),
          function(object, ...){

              ## Pull out current.ob into x.

              x <- object@current.ob

              ## Takes out the bids.

              x <- x[x[["type"]] == "BID",]

              ## Return -1 if x is empty, i.e. there are no bids.

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

## Returns a vector with the price and size of the best ask at the
## current order book state. See comments above. Code is identical
## except we find min instead of max price.

setMethod("best.ask",
          signature(object = "orderbook"),
          function(object, ...){

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

## Show basic information about the order book at its current
## state. .prettify is a helper function to make the output print
## nicely.

setMethod("show",
          signature(object = "orderbook"),
          function(object){
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


## Plot calls helper plot methods. See orderbook.plot.R for more
## details.

setMethod("plot",
          signature(x = "orderbook"),
          function(x, bounds = 0.1, n = 10, type = "n"){

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

## Calls the .plot.trade method. See orderbook.plot.R. Might get rid
## of this altogether.

setMethod("plotTrade",
          signature(x = "orderbook"),
          function(x){
              .plot.trade(x)
          }
          )

## Displays summary information.

setMethod("summary",
          signature(object = "orderbook"),
          function(object){
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

              inside.market(object)
              cat("\n")
          }
          )

## Displays the price levels and sizes. n specifies the number of rows to be
## displayed for ask and bid.

setMethod("display",
          signature(object = "orderbook"),
          function(object, n = 5, ...){

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

## Add an order, you need price, size, type. You should probably
## specify ID but if you don't the orderbook will add it for you
## anyways and automatically make it 1 greater than the current
## greatest ID.

setMethod("add.order",
          signature(object = "orderbook"),
          function(object, price, size, type, time = NULL, id = NULL,
                   mine = NULL, ...){

              ## Some checks to make sure the order is
              ## valid. Specifically, making sure the price and size
              ## are positive and a valid type has been specified.

              stopifnot(price > 0 & size > 0)
              stopifnot(type == "ASK" | type == "BID")

              ## Pull out the order book data frame.

              x <- object@current.ob

              ## If user doesn't specify a time make the new time
              ## 1000ms after the current time

              if(is.null(time)){
                  new.time <- object@current.time + 1000
              } else {
                  new.time <- time
              }

              ## If user doesn't specify an ID make the new id 1
              ## larger than the largest ID.

              if(is.null(id) & nrow(x) != 0){
                  id <- max(as.numeric(x[["id"]])) + 1
              } else if(is.null(id)){
                  id <- 1
              }

              ## Create the new order as a data frame and name it.

              new.order <- data.frame(price, size, type, new.time, id, mine)
              names(new.order) <- c("price", "size", "type", "time",
                                    "id", "mine")

              ## Rbind it to current.ob.

              x <- do.call(rbind, list(x, new.order))

              ## Store it into object and return the object.

              object@current.ob <- x
              object@current.time <- new.time

              invisible(object)

          }
          )

## Replace the size of an order.. You need to specify ID and new size.

setMethod("replace.order",
          signature(object = "orderbook"),
          function(object, id, size, ...){

              ## If size is 0 just remove the order.

              if(identical(size, 0)){

                    invisible(remove.order(object, id))

              } else {

                  ## Pull out the order book data frame.

                   x <- object@current.ob

                   ## Make sure the new size isn't greater than the
                   ## current size.

                   tmp.size <- x[["size"]][x[["id"]] == id]
                   if(tmp.size < size){

                       print("Warning size greater than current size")

                   } else {

                       ## Do the replacement.

                       x[["size"]][x[["id"]] == id] <- min(size,
                                    tmp.size)

                       ## Store the new order book data frame then
                       ## return the object.

                       object@current.ob <- x
                       invisible(object)

                   }
               }
          }
          )

## Runs a market order. If there is not enough volume to fill the
## order will be partially filled and cancelled. Might be removed.

setMethod("market.order",
          signature(object = "orderbook"),
          function(object, size, type, ...){

              stopifnot(type == "BUY" | type == "SELL")
              stopifnot(size > 0)

              x <- object@current.ob

              ## Take out ask and bid dataframes.

              ask <- x[x[["type"]] == "ASK",]
              bid <- x[x[["type"]] == "BID",]

              ## If its a buy, then remove orders until you run out
              ## then replace the last one.

              if(type == "BUY" & nrow(ask) > 0){

                  ## Order ask by lowest price, time

                  ask <- ask[order(ask[["price"]], ask[["time"]]),]

                  while(size > 0 & nrow(ask) > 0){

                      ## Decrement size.

                      size = size - ask[["size"]][1]

                      ## Either remove or replace.

                      if(size >= 0){

                          object <- remove.order(object,
                                                 ask[["id"]][1])

                          ask <- ask[-1,]

                      } else if(size < 0){

                          object <- replace.order(object,
                                                  ask[["id"]][1],
                                                  abs(size))
                      }
                  }

                  ## See above

              } else if(type == "SELL" & nrow(bid) > 0){

                  bid <- bid[order(bid[["time"]]),]

                  bid <- bid[order(bid[["price"]], decreasing =
                                   TRUE),]

                  while(size > 0 & nrow(bid) > 0){

                      size <- size - bid[["size"]][1]

                      if(size >= 0){

                          object <- remove.order(object,
                                                 bid[["id"]][1])

                          bid <- bid[-1,]

                      } else if(size < 0){

                          object <- replace.order(object,
                                                  bid[["id"]][1],
                                                  abs(size))
                      }
                  }
              }
              invisible(object)
          }
          )

## Returns the number of bid price levels.

setMethod("bid.price.levels",
          signature(object = "orderbook"),
          function(object, ...) {
              x <- object@current.ob

              ## Pull out the bids.

              x <- x[x[["type"]]=="BID",]

              ## Use table to count.

              return(length(table(x[["price"]], exclude = NA)))
          }
          )

## Returns the number of ask price levels. See above.

setMethod("ask.price.levels",
          signature(object = "orderbook"),
          function(object, ...) {
              x <- object@current.ob

              x <- x[x[["type"]]=="ASK",]

              return(length(table(x[["price"]], exclude = NA)))

          }
          )

## Returns the total number of price levels.

setMethod("total.price.levels",
          signature(object = "orderbook"),
          function(object, ...) {

              return(bid.price.levels(object) +
                     ask.price.levels(object))

          }
          )

## Returns the number of bid orders.

setMethod("bid.orders",
          signature(object = "orderbook"),
          function(object, ...) {
              x <- object@current.ob

              ## Isolate bids from order book.

              x = x[x[["type"]] == "BID",]

              ## Take care of NAs by using max. Count using nrow.

              return(max(0, nrow(x)))
          }
          )

## Returns the number of ask orders. See above.

setMethod("ask.orders",
          signature(object = "orderbook"),
          function(object, ...) {
              x <- object@current.ob

              x = x[x[["type"]] == "ASK",]

              return(max(0, nrow(x)))
          }
          )

## Returns the total number of orders.

setMethod("total.orders",
          signature(object = "orderbook"),
          function(object, ...) {

              return(ask.orders(object) + bid.orders(object))

          }
          )

## Returns the midpoint value, which is just the simple average of the
## best bid and ask.

setMethod("mid.point",
          signature(object = "orderbook"),
          function(object, ...) {

              return((best.bid(object)[1] + best.ask(object)[1])/2)

          }
          )

## Returns a data frame with a row for the best ask and a row for the
## best bid.  The columns are price, size, and type.

setMethod("inside.market",
          signature(object = "orderbook"),
          function(object, invis = FALSE, ...){

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

## Returns the spread.

setMethod("spread",
          signature(object = "orderbook"),
          function(object, ...){

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

## Remove an order by ID.

setMethod("remove.order",
          signature(object = "orderbook"),
          function(object, id, ...){

              x <- object@current.ob

              ## Remove from current.ob

              x <- x[x[["id"]] != id,]

              ## Store it back into the object and return the object.

              object@current.ob <- x
              invisible(object)
          }
          )

## Jump to trade index. Need to read in the entire orderbook first
## though to build trade.data. Might remove.

setMethod("view.trade",
          signature(object = "orderbook"),
          function(object, n = 1){
              x <- object@trade.data

              currentindex <- object@file.index
              nextindex <- sort(as.numeric(names(x)))[n]

              n <- nextindex - currentindex

              invisible(read.orders(object, n))
          }
          )

## Create Trellis objects that are used for the animation and save
## them using tempfile(). Put the location of tempfile() in
## orderbook@animation. Can view by message or seconds.

setMethod("load.animation",
          signature(object = "orderbook"),
          function(object, from, to, by = "sec", bounds =
                   0.02){

              if(isTRUE(by %in% "sec")){

                  ## Create the vector of times by converting from and
                  ## to POSIX objects and then using sequence to
                  ## create a sequence between the two times.

                  from <- as.POSIXlt(from, format = "%H:%M:%S")
                  to <- as.POSIXlt(to, format = "%H:%M:%S")
                  time <- seq.POSIXt(from.time, to, "sec")

                  ## Format to get rid of the dates.

                  time <- format(time, format ="%H:%M:%S")

                  ## Run helper function to do the actual creation of
                  ## the Trellis objects.

                  invisible(.animate.seconds(object, time, bounds))

              } else if(isTRUE(by %in% "msg")){

                  ## Get object to the correct message number.

                  object <- read.orders(object, from - object@file.index)
                  n <- to - from

                  ## Run helper function to do actual creation of the
                  ## Trellis objects.

                  invisible(.animate.orders(object, n, bounds))

              }
          }
          )

## Load trade animation given a trade number.

setMethod("load.trade.animation",
          signature(object = "orderbook"),
          function(object, tradenum, before = 30, after = 30, by =
                   "both", bounds = 0.02){

              ## Extract the desired trade from my trades.

              trade <- object@my.trades
              trade <- trade[tradenum,]

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

                  from <- tradetime - before
                  to <- tradetime + after

                  ## Generate our vector of times.

                  time <- seq.POSIXt(from, to, "sec")
                  time <- format(time, format ="%H:%M:%S")

                  object <- .animate.seconds(object, time, bounds)
              }

              if(isTRUE(by %in% "msg") | isTRUE(by %in% "both")){

                  ## Obtain the row by pulling the first column.

                  traderow <- trade[[1]]

                  ## n is the row 30 messages before the specified trade.

                  n <- traderow - before - object@file.index

                  ## Create an orderbook at message n.

                  object <- read.orders(object, n)

                  ## Animate the orderbook from n to before + after messages after n.

                  object <- .animate.orders(object, before + after, bounds)

              }

              ## Set a new trade.index and return the object.

              object@trade.index <- tradenum
              invisible(object)

          }
          )

## Loads trade animation for the current trade index then increments
## it by 1.

setMethod("load.next.trade",
          signature(object = "orderbook"),
          function(object, before = 30, after = 30, by = "both",
                   bounds = .02){

              object <- load.trade.animation(object,
                                             object@trade.index,
                                             before, after, by,
                                             bounds)

              object@trade.index <- object@trade.index + 1

              invisible(object)
          }
          )

## Loads trade animation for current trade index - 1 then decrements
## it by 1.

setMethod("load.previous.trade",
          signature(object = "orderbook"),
          function(object, before = 30, after = 30, by = "both",
                   bounds = .02){

              if(object@trade.index > 1){

                  object <- load.trade.animation(object,
                                                 object@trade.index - 1,
                                                 before, after, by,
                                                 bounds)

                  object@trade.index <- object@trade.index - 1

              }

              invisible(object)

              }
              )

## Sort my.trades by a user specified column and resets the
## trade.index to 1.

setMethod("sort",
          signature(x = "orderbook"),
          function(x, by, decreasing = TRUE){

              tmp <- x@my.trades

              x@my.trades <- tmp[order(tmp[[by]], decreasing =
                                       decreasing),]

              x@trade.index <- 1

              invisible(x)
          }
          )

## Clears current.ob, does not clear trade data, my trades, or
## trade.index.

setMethod("reset",
          signature(object = "orderbook"),
          function(object){

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

## Animate is to be used in conjunction with load.trade.animation or
## load.animation. After an animation is created its location is
## stored in object@animation. This function loads the animation and
## uses a for loop to play through it.

setMethod("animate",
          signature(object = "orderbook"),
          function(object, pause = 0.25, type = "sec"){

              ## Load the trade animation stored in object@animation
              ## according to type.

              filename <- object@animation[[type]]
              load(filename)

              ## Remove "name" slot from the name vector.

              name <- name[-length(name)]

              ## Loop through name to print all the objects.

              for(i in 1:length(name)){

                  print(get(name[i]))

                  ## Pause during each plot.

                  Sys.sleep(pause)
              }

          }
          )

## Accesses the orders at the specified price level.

setMethod("[",
          signature(x = "orderbook", i = "character"),
          function(x, i){

              ## Extract the current order book and cast i as a
              ## number.

              current.ob <- x@current.ob
              i = as.numeric(i)

              ## Pull out all rows where the price is equal to i and
              ## erase the rownames.

              tmp <- current.ob[current.ob$price == i,]
              rownames(tmp) <- NULL

              ## Return tmp.

              return(tmp)
          }
          )

## Initialize Trades--calculate midpoint return/trade weighted average
## price return for all trades in my.trades. Takes the object as well
## as a vector of time in seconds to decide the times for finding the
## returns.

setMethod("initialize.trades",
          signature(object = "orderbook"),
          function(object, time = c(5, 300)){


              ## Get my trades.

              mytrades <- object@my.trades

              ## Create a list to store the trade returns.

              tradereturns = list()

              ## For each of my trades

              for(i in 1:nrow(mytrades)){

                  ## Find its row within the data file

                  row <- mytrades$row[i]

                  ## Given that information find the midpoint and twap

                  tmp <- c(.midpoint.return(object, row, time),
                               .twap.return(object, row, time))

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





