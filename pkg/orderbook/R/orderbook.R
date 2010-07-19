################################################################################
##
##
## orderbook.R: functions of the orderbook object
##
## limitob is free software: you can redistribute it and/or modify it
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
## along with limitob.  If not, see <http://www.gnu.org/licenses/>.
################################################################################

setClass("orderbook", representation(current.ob   = "data.frame",
                                     current.time = "numeric",
                                     file         = "character",
                                     file.index   = "numeric",
                                     ob.data      = "hash",
                                     trade.data   = "hash",
                                     my.trades    = "hash",
                                     animation    = "list"
                                     ),

         prototype(current.ob   = data.frame(),
                   current.time = 0,
                   file		= character(),
                   file.index   = 0,
                   ob.data      = hash(),
                   trade.data   = hash(),
                   my.trades    = hash(),
                   animation = list(sec = character(), msg =
                   character())
                   )
         )

## Reads the next n orders from the file. Use negative n to go
## backwards, but this really just reads everything over again.

setMethod("read.orders",
          signature(object = "orderbook"),
          function(object, n = 1000){
              if(n > 0){
                  invisible(.read.orders(object, n))
              } else {
                  n <- object@file.index + n
                  object <- reset(object)
                  invisible(.read.orders(object, n))
              }

          }
          )

## Reads orders from the file until the time specified.

setMethod("read.time",
           signature(object = "orderbook"),
           function(object, n){

               ## If the time you are reading is greater than current
               ## time there is no reason to start from the beginning.
               ## This takes care of that.

               if(.to.ms(n) > object@current.time){

                   ## get.time.row finds the row in the file.

                   n <- .get.time.row(object@file, .to.ms(n),
                                      object@file.index)

                   n <- n - object@file.index

                   invisible(read.orders(object, n))
               } else {
                   n <- .get.time.row(object@file, .to.ms(n))
                   object <- reset(object)
                   invisible(read.orders(object, n))
               }
           }
           )




## Returns a vector with the price and size of the best bid order at top
## priority.

setMethod("best.bid",
          signature(object = "orderbook"),
          function(object, ...){
              x <- object@current.ob

              ## Takes out the bids.

              x <- x[x[["type"]] == "BID",]

              ## Sorts by time.

              x <- x[order(x[["time"]]),]

              ## Gets indices for the best bid.

              index <- x[["price"]] == max(x[["price"]])

              price <- x[["price"]][index]
              size <- x[["size"]][index]

              ## Return -1 (no best bid) if x is empty, otherwise return named
              ## vector of price and size.

              if(identical(nrow(x), 0)){
                  return(-1)
              } else {
                  return(c(price = price[1], size = size[1]))
              }
          }
          )

## Returns a vector with the price and size of the best ask order at top
## priority. See above for comments

setMethod("best.ask",
          signature(object = "orderbook"),
          function(object, ...){
              x <- object@current.ob

              x <- x[x[["type"]] == "ASK",]

              x <- x[order(x[["time"]]),]

              index <- x[["price"]] == min(x[["price"]])

              price <- x[["price"]][index]
              size <- x[["size"]][index]

              if(identical(nrow(x), 0)){
                  return(-1)
              } else {
                  return(c(price = price[1], size = size[1]))
              }

          }
          )

## Show basic information about the order book.

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


## Plot, basically just calls the plot method. See orderbook.plot.R.

setMethod("plot",
          signature(x = "orderbook"),
          function(x, bounds = 0.1, n = 10, type = "n"){
              if(isTRUE(type %in% "n")){

                  tmp <- .plot.ob(x, bounds)
                  print(tmp)

              } else if(isTRUE(type %in% "s")){

                  .plot.side.ob(x, n)

              } else if(isTRUE(type %in% "o")){

                  tmp <- .plot.orders.ob(x, bounds)
                  print(tmp)

              } else if(isTRUE(type %in% "sd")){

                  tmp <- .supply.demand.plot(x, bounds)
                  print(tmp)

              } else {

                  print("Invalid type")

              }
          }
          )

## Basically just calls the .plot.trade method. See orderbook.plot.R.

setMethod("plotTrade",
          signature(x = "orderbook"),
          function(x){
              .plot.trade(x)
          }
          )


## Takes ID as input, returns vector of price and size for that ID.

setMethod("get.order.info",
          signature(object = "orderbook"),
          function(object, id, ...){
              x <- object@current.ob

              ## Pulls out information

              tmp.price <- x[["price"]][x[["id"]] == id]
              tmp.size <- x[["size"]][x[["id"]] == id]

              return(c(price = tmp.price, size = tmp.size))
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

              ## Midpoint--format it if there are less than 3 decimals

              mid <- mid.point(object)
              check <- mid - floor(mid)
              if(!identical(nchar(check), 5))
                 formatC(mid, format = "f", digits = 2)

              cat("Mid point: ", mid, "\n")
              cat("-----------------------------\n")
              cat("Inside market \n \n")

              ## Calls inside.market function which prints it

              inside.market(object)
              cat("\n")
          }
          )

## Displays the price levels and sizes. n specifies the number of rows to be
## displayed for ask and bid. Short = FALSE returns the data frame sorted
## in decreasing order by price so you can see all individual orders.

setMethod("display",
          signature(object = "orderbook"),
          function(object, n = 5, short = TRUE, ...){
              if(short){
                  x <- .combine.size(object, Inf)

                  ## Create ask and bid data frames

                  ask <- x[x[["type"]] == "ASK",]
                  bid <- x[x[["type"]] == "BID",]

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
                      ## 10 million for this to work

                      size = .prettify(bid[["size"]][i], "s")
                      if(nchar(size) <= 7){
                          space = rep(" ", 7 - nchar(size))
                      } else {
                          space = ""
                      }

                      space = paste(space, collapse = "")

                      size = paste(space, size, sep = "")

                      ## Actually printing it out

                      cat(size, "\t",
                          .prettify(bid[["price"]][i]), "\n")
                  }
                  cat("---------------------------------------------\n")
                  cat("Bid Size \t Price\n")
                  invisible(object)
              } else {

                  ## Print the whole thing short == FALSE

                  x <- object@current.ob
                  x <- x[order(x[["price"]], decreasing = TRUE),]
                  return(x)
              }


          }
          )

## Add an order, you need price, size, type. You should probably
## specify ID but if you don't the orderbook will add it for you
## anyways and automatically make it 1 greater than the current
## greatest ID.

setMethod("add.order",
          signature(object = "orderbook"),

          function(object, price, size, type, time = NULL, id = NULL,
          ...){

              ## Some checks to make sure the order is valid

              stopifnot(price > 0 & size > 0)
              stopifnot(type == "ASK" | type == "BID")


              x <- object@current.ob
              y <- copy(object@ob.data)

              ## If user doesn't specify a time make the new time
              ## 1000ms after the current time

              if(is.null(time)){
                  new.time <- object@current.time + 1000
              } else {
                  new.time <- time
              }

              ## If user doesn't specify an ID make the new id
              ## 1 larger than the largest

              if(is.null(id) & nrow(x) != 0){
                  id <- max(as.numeric(x[["id"]])) + 1
              } else if(is.null(id)){
                  id <- 1
              }

              ## Create the new order

              new.order <- data.frame(price, size, type, new.time, id)
              names(new.order) <- c("price", "size", "type",
                                    "time", "id")

              ## Bind it to current.ob and add it to ob.data

              x <- rbind(x, new.order)
              y[id] <- c(time, id, price, size, type)

              ## Store it into object

              object@current.ob <- x
              object@current.time <- new.time
              object@ob.data <- y

              invisible(object)

          }
          )

## Go to next trade after current time.

setMethod("next.trade",
          signature(object = "orderbook"),
          function(object){
              ## .get.next.trade finds the next trade

              n <- .get.next.trade(object@file, object@file.index)
              n <- n - object@file.index
              invisible(read.orders(object, n))
          }
          )

## Go to the first trade to occur before the current time.

setMethod("previous.trade",
          signature(object = "orderbook"),
          function(object){
              ## Use the trade.data to find the row of the previous
              ## trade

              x <- object@trade.data

              trade.index <- length(x)

              nextindex <- sort(as.numeric(names(x)))[trade.index]
              nextindex <- nextindex - object@file.index

              invisible(read.orders(object, nextindex))
          }
          )


## Replace an order. You need to specify ID and size.

setMethod("replace.order",
          signature(object = "orderbook"),
          function(object, id, size, ...){

              ## If size is 0 just remove the order.

              if(identical(size, 0)){
                    invisible(remove.order(object, id))
              } else {
                   x <- object@current.ob
                   y <- copy(object@ob.data)

                   stopifnot(size > 0)

                   ## Make sure the new size isn't greater than the
                   ## current size.

                   tmp.size <- x[["size"]][x[["id"]] == id]
                   if(tmp.size < size){

                       print("Warning size greater than current size")

                   } else {

                       ## Do the replacement

                       x[["size"]][x[["id"]] == id] <- min(size,
                                         tmp.size)

                       y[[as.character(id)]][4] <- size

                       object@current.ob <- x
                       object@ob.data <- y
                       invisible(object)

                   }
               }
          }
          )

## Runs a market order. If there is not enough volume to fill the
## order will be partially filled and cancelled.

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

              ## Pull out the bids, then split by price.

              x <- x[x[["type"]]=="BID",]

              ## Use table to count.

              return(length(table(x[["price"]], exclude = NA)))
          }
          )

## Returns the number of ask price levels.

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

              x = x[x[["type"]] == "BID",]

              ## Take care of NAs by using max

              return(max(0, nrow(x)))
          }
          )

## Returns the number of ask orders.

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

## Returns the midpoint value.

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
          function(object, invis = FALSE, ...) {

              x <- .combine.size(object, .05)

              ## Create ask and bid data frames.

              ask <- x[x[["type"]] == "ASK",]
              bid <- x[x[["type"]] == "BID",]

              ask <- c(price = ask[["price"]][1],
                       size = ask[["size"]][1])

              bid <- c(price = bid[["price"]][nrow(bid)],
                       size = bid[["size"]][nrow(bid)])

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
          function(object, ...) {

              ask = best.ask(object)
              bid = best.bid(object)

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
              y <- copy(object@ob.data)

              ## Remove from ob.data and current.ob

              x <- x[x[["id"]] != id,]
              y[id] <- NULL

              object@current.ob <- x
              object@ob.data <- y
              invisible(object)
          }
          )

## Jump to trade index. Need to read in the entire orderbook first
## though to build trade.data.

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

## Preload the animations.

setMethod("load.animation",
          signature(object = "orderbook"),
          function(object, from, to, by = "sec", bounds = 0.05)
      {
              if(isTRUE(by %in% "sec")){

                  ## Create the vector of times

                  from <- as.POSIXlt(from, format = "%H:%M:%S")
                  to <- as.POSIXlt(to, format = "%H:%M:%S")
                  time <- seq.POSIXt(from, to, by)
                  time <- format(time, format ="%H:%M:%S")

                  ## Run helper function

                  invisible(.animate.seconds(object, time, bounds))

              } else if(isTRUE(by %in% "msg")){

                  ## Get tmp.ob to the correct order

                  tmp.ob <- copy(object)
                  tmp.ob <- read.orders(tmp.ob, from - tmp.ob@file.index)
                  n <- to - from

                  ## Run helper function

                  invisible(.animate.orders(tmp.ob, n, bounds, object))

              }
          }
          )

## Load trade animation.

setMethod("load.trade.animation",
          signature(object = "orderbook"),
          function(object, before = 30, after = 30, by = "sec", bounds
                   = 0.05){

              if(isTRUE(by %in% "sec")){

                  ## Find row of next trade.

                  traderow <- .get.next.trade(object@file,
                                              object@file.index)

                  ## Find the time at that row.

                  tradetime <- .get.row.time(object@file, traderow)

                  ## Find the time 30 seconds before and 30 seconds after that time.

                  tradetime <- .to.time(tradetime)

                  tradetime <- as.POSIXlt(tradetime, format = "%H:%M:%S")

                  from <- tradetime - before
                  to <- tradetime + after

                  ## Generate our vector of times.

                  time <- seq.POSIXt(from, to, by)
                  time <- format(time, format ="%H:%M:%S")

                  invisible(.animate.seconds(object, time, bounds))

              } else if(isTRUE(by %in% "message")){

                  ## Find row of next trade.

                  traderow <- .get.next.trade(object@file,
                                              object@file.index)

                  ## Create a copy of the object

                  tmp.ob <- copy(object)

                  ## We want to read to x orders before the tradrow.

                  n <- traderow - before - tmp.ob@file.index

                  ## Read to 30 orders before the traderow

                  tmp.ob <- read.orders(tmp.ob, n)

                  invisible(.animate.orders(tmp.ob, before + after, bounds, object))
              }
          }
          )

## Reset to beginning, does not clear trade data or my trades.

setMethod("reset",
          signature(object = "orderbook"),
          function(object){

              ## Clear the hash

              clear(object@ob.data)

              current.ob <- data.frame(numeric(0), numeric(0),
                                       character(0), numeric(0), character(0))

              names(current.ob) <- c("price", "size", "type", "time",
                                     "id")

              invisible(new("orderbook",
                            current.ob   = current.ob,
                            current.time = 0,
                            file         = object@file,
                            file.index   = 0,
                            ob.data      = hash(),
                            trade.data   = object@trade.data,
                            my.trades    = object@my.trades))
          }
          )

## Animate, run preload first to generate the .Rdata file

setMethod("animate",
          signature(object = "orderbook"),
          function(object, pause = 0.25, type = "sec"){

              ## "Animates" using a for loop.

              filename <- object@animation[[type]]
              load(filename)
              name <- name[-length(name)]

              for(i in 1:length(name)){
                  print(get(name[i]))
                  Sys.sleep(pause)
              }

          }
          )

## Pulls out the orders at the specified price level

setMethod("[",
          signature(x = "orderbook", i = "character"),
          function(x, i){

              current.ob <- x@current.ob
              i = as.numeric(i)

              tmp <- current.ob[current.ob$price == i,]
              rownames(tmp) <- NULL
              return(tmp)
          }
          )

## Creates a copy of the orderbook. This is necessary since hash
## requires copies to be made.

setMethod("copy",
          signature(x = "orderbook"),
          function(x){
              if(length(x@ob.data) > 0)
                  x@ob.data <- copy(x@ob.data)
              else
                  x@ob.data <- hash()

              invisible(x)
          }
          )

## Midpoint Return, automatically finds the midpoint return for the
## selected message row number for a vector of time in seconds,
## e.g. c(5, 10, 60, 120) means find the midpoint return for 5s, 10s,
## 1 min, 2 min after the trade.

