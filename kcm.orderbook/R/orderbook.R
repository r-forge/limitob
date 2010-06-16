################################################################################
##
## $Id: orderbook.R 1622 2010-02-18 17:50:34Z enos $
##
## Result object for an orderbook
##
################################################################################

setClass("orderbook", representation(current.ob   = "data.frame",
                                     current.time = "numeric",
                                     ob.names     = "character",
                                     feed         = "character",
                                     feed.index   = "numeric",
                                     ob.data      = "matrix",
                                     current.pos  = "numeric",
                                     trade.data   = "hash",
                                     trade.index  = "numeric",
                                     ids = "hash"
                                     ),

         prototype(current.ob   = data.frame(),
                   current.time = numeric(),
                   ob.names     = character(),
                   feed		= character(),
                   feed.index   = 0,
                   ob.data      = matrix(),
                   current.pos  = numeric(),
                   trade.data   = hash(),
                   trade.index  = 1,
                   ids          = hash()
                   )
         )

## Reads the next n orders from the feed. Use negative n to go
## backwards, but this really just reads everything over again.

setMethod("read.orders",
          signature(object = "orderbook"),
          function(object, n = 1000, ...){
              if(n > 0){
                  invisible(.read.orders(object, n))
              } else {
                  n = object@feed.index + n
                  object = reset(object)
                  invisible(.read.orders(object, n))
              }

          }
          )

## Reads orders from the feed until the time specified.

setMethod("read.time",
           signature(object = "orderbook"),
           function(object, n, ...){

               ## If the time you are reading is greater than current
               ## time there is no reason to start from the beginning.
               ## This takes care of that.

               if(.to.ms(n) > object@current.time){
                   n = .get.time.row(object@feed, .to.ms(n),
                   object@feed.index)

                   n = n - object@feed.index

                   invisible(read.orders(object, n))
               } else {
                   n = .get.time.row(object@feed, .to.ms(n))
                   object = reset(object)
                   invisible(read.orders(object, n))
               }
           }
           )




## Returns a vector with the price and size of the best bid order at top
## priority.

setMethod("best.bid",
          signature(object = "orderbook"),
          function(object, ...){
              x = object@current.ob
              ob.names = object@ob.names

              ## Takes out the bids.

              x = x[x[[ob.names[3]]] == ob.names[7],]

              ## Sorts by time.

              x = x[order(x[[ob.names[4]]]),]

              ## Gets indices for the best bid.

              index = x[[ob.names[1]]] == max(x[[ob.names[1]]])

              price = x[[ob.names[1]]][index]
              size = x[[ob.names[2]]][index]

              ## Return 0 (no best bid) if x is empty, otherwise return named
              ## vector of price and size.

              if(nrow(x) == 0){
                  return(0)
              } else {
                  return(c(price = price[1], size = size[1]))
              }
          }
          )

## Returns a vector with the price and size of the best ask order at top
## priority.

setMethod("best.ask",
          signature(object = "orderbook"),
          function(object, ...){
              x = object@current.ob
              ob.names = object@ob.names

              x = x[x[[ob.names[3]]] == ob.names[6],]

              index = x[[ob.names[1]]] == min(x[[ob.names[1]]])

              price = x[[ob.names[1]]][index]
              size = x[[ob.names[2]]][index]
              if(nrow(x) == 0){
                  return(0)
              } else {
                  return(c(price = price[1], size = size[1]))
              }

          }
          )

## Show basic information about the order book.


setMethod("show",
          signature(object = "orderbook"),
          function(object){
              cat("An object of class limitob\n")
              cat("--------------------------\n")
              cat("Current orderbook time:   ", .to.time(object@current.time), "\n")
              cat("Feed Index:               ", object@feed.index, "\n")
              cat("Number of Bids:           ",
                  .prettify(bid.orders(object), "s"), "\n")
              cat("Number of Asks:           ",
                  .prettify(ask.orders(object), "s"), "\n")
              cat("Total Orders:             ",
                  .prettify(total.orders(object), "s"), "\n")
          }
          )


## Plot, basically just calls the plot method. See orderbook.plot.R.

setMethod("plot",
          signature(x = "orderbook"),
          function(x, bounds = 0.1, ...){
              .plot.lattice.order.book(x, bounds)
          }
          )

## Takes ID as input, returns vector of price and size for that ID.

setMethod("get.order.info",
          signature(object = "orderbook"),
          function(object, id, ...){
              x = object@current.ob
              ob.names = object@ob.names
              tmp.price = x[[ob.names[1]]][x[[ob.names[5]]] == id]
              tmp.size = x[[ob.names[2]]][x[[ob.names[5]]] == id]

              return(c(price = tmp.price, size = tmp.size))

          }
          )

## Displays summary information.

setMethod("summary",
          signature(object = "orderbook"),
          function(object){
              cat("\nCurrent time is",
                      .to.time(object@current.time), "\n\n")
              cat("ASK price levels:  ", ask.price.levels(object), "\n")
              cat("BID price levels:  ", bid.price.levels(object), "\n")
              cat("Total price levels:", total.price.levels(object), "\n\n")
              cat("-----------------------------\n")
              cat("ASK orders:        ", ask.orders(object), "\n")
              cat("BID orders:        ", bid.orders(object), "\n")
              cat("Total orders:      ", total.orders(object), "\n\n")
              cat("-----------------------------\n")
              cat("Spread:            ", .prettify(spread(object)), "\n\n")
              cat("Mid point:         ",
              formatC(mid.point(object), format = "f", digits = 3), "\n \n")
              cat("-----------------------------\n")
              cat("Inside market \n \n");
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
                  x = .combine.size(object, Inf)
                  ob.names = object@ob.names
                  ask = x[x[[ob.names[3]]] == ob.names[6],]
                  bid = x[x[[ob.names[3]]] == ob.names[7],]
                  cat("\nCurrent time is",
                      .to.time(object@current.time), "\n\n")
                  cat("\t\t Price \t\t Ask Size\n")
                  cat("---------------------------------------------\n")
                  for(i in rev(1:min(n, nrow(ask)))){
                      cat("\t\t",
                          .prettify(ask[[ob.names[1]]][i]), "\t\t",
                          .prettify(ask[[ob.names[2]]][i], "s"), "\n")
                  }

                  cat("---------------------------------------------\n")
                  for(i in rev(max(1, nrow(bid) - n):nrow(bid))){
                      cat(.prettify(bid[[ob.names[2]]][i], "s"), "\t\t",
                          .prettify(bid[[ob.names[1]]][i]), "\n")
                  }
                  cat("---------------------------------------------\n")
                  cat("Bid Size \t Price\n")
                  invisible(object)
              } else {
                  x = object@current.ob
                  ob.names = object@ob.names
                  x = x[order(x[[ob.names[1]]], decreasing = TRUE),]
                  return(x)
              }


          }
          )

## Add an order, you need price, size, type. You should probably specify ID
## but if you don't the orderbook will add it for you anyways and automatically
## make it 1 greater than the current greatest ID.

setMethod("add.order",
          signature(object = "orderbook"),
          function(object, price, size, type, time = NULL, id = NULL, ...){

              ob.names = object@ob.names

              stopifnot(price>0 & size > 0)
              stopifnot(type == ob.names[6] | type == ob.names[7])


              x = object@current.ob


              ob.names = object@ob.names

              if(is.null(time)){
                  new.time = object@current.time
              } else {
                  new.time = time
              }

              if(is.null(id) & nrow(x) != 0){
                  id = max(as.numeric(x[[ob.names[5]]])) + 1
              } else if(is.null(id)){
                  id = 1
              }

              new.order = data.frame(price, size, type, new.time, id)
              names(new.order) = c(ob.names[1], ob.names[2], ob.names[3],
                   ob.names[4], ob.names[5])

              x = rbind(x, new.order)

              ## Adding new data frame

              object@current.ob <- x
              object@current.time <- new.time

              invisible(object)

          }
          )

## Replace an order. You need to specify ID and size.

setMethod("replace.order",
          signature(object = "orderbook"),
          function(object, id, size, ...){
              if(size == 0){
                    invisible(remove.order(object, id))
              } else {
                   x = object@current.ob
                   ob.names = object@ob.names

                   stopifnot(size > 0)

                   tmp.size = x[[ob.names[2]]][x[[ob.names[5]]] == id]
                   if(tmp.size < size){
                       print("Warning size greater than current size")
                   } else {
                       x[[ob.names[2]]][x[[ob.names[5]]] == id] = min(size, tmp.size)

                       object@current.ob <- x
                       invisible(object)

                   }
             }
          }
          )

## Runs a market order. If there is not enough volume to fill the order will
## be partially filled and cancelled.

setMethod("market.order",
          signature(object = "orderbook"),
          function(object, size, type, ...){

              stopifnot(type == "BUY" | type == "SELL")
              stopifnot(size > 0)

              x = object@current.ob
              ob.names = object@ob.names

              tmp.ask = x[x[[ob.names[3]]] == ob.names[6],]
              tmp.bid = x[x[[ob.names[3]]] == ob.names[7],]

              if(type == "BUY" & nrow(tmp.ask) > 0){
                  tmp.ask = tmp.ask[order(tmp.ask[[ob.names[1]]],
                  tmp.ask[[ob.names[4]]]),]

                  while(size > 0 & nrow(tmp.ask) > 0){
                      size = size - tmp.ask[[ob.names[2]]][1]
                      if(size >= 0){
                          tmp.ask = tmp.ask[-1,]
                      } else if(size < 0){
                          tmp.ask[[ob.names[2]]][1] =  abs(size)
                      }
                  }

              } else if(type == "SELL" & nrow(tmp.bid) > 0){

                  tmp.bid = tmp.bid[order(tmp.bid[[ob.names[1]]],
                  decreasing = TRUE),]
                  tmp.bid = tmp.bid[order(tmp.bid[[ob.names[4]]]),]

                  while(size > 0 & nrow(tmp.ask) > 0){
                      size = size - tmp.bid[[ob.names[2]]][1]
                      if(size >= 0){
                          tmp.bid = tmp.bid[-1,]
                      } else if(size < 0){
                          tmp.bid[[ob.names[2]]][1] =  abs(size)
                      }
                  }
              }

              x = rbind(tmp.ask, tmp.bid)

              object@current.ob <- x
              invisible(object)

          }
          )




## Returns the number of bid price levels.

setMethod("bid.price.levels",
          signature(object = "orderbook"),
          function(object, ...) {
              x = object@current.ob
              ob.names = object@ob.names
              x = x[x[[ob.names[3]]]==ob.names[7],]
              by.type = split(x, x[[ob.names[1]]])
              return(max(0, length(by.type)))
          }
          )

## Returns the number of ask price levels.

setMethod("ask.price.levels",
          signature(object = "orderbook"),
          function(object, ...) {
              x = object@current.ob
              ob.names = object@ob.names
              x = x[x[[ob.names[3]]]==ob.names[6],]
              by.type = split(x, x[[ob.names[1]]])
              return(max(0, length(by.type)))
          }
          )

## Returns the total number of price levels.

setMethod("total.price.levels",
          signature(object = "orderbook"),
          function(object, ...) {
              return(bid.price.levels(object) + ask.price.levels(object))
          }
          )

## Returns the number of bid orders.

setMethod("bid.orders",
          signature(object = "orderbook"),
          function(object, ...) {
              x = object@current.ob
              ob.names = object@ob.names
              by.type = split(x, x[[ob.names[3]]])
              return(max(0, nrow(by.type[[ob.names[7]]])))
          }
          )

## Returns the number of ask orders.

setMethod("ask.orders",
          signature(object = "orderbook"),
          function(object, ...) {
              x = object@current.ob
              ob.names = object@ob.names
              by.type = split(x, x[[ob.names[3]]])
              return(max(0, nrow(by.type[[ob.names[6]]])))
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

## Returns a data frame with a row for the best ask and a row for the best bid.
## The columns are price, size, and type.

setMethod("inside.market",
          signature(object = "orderbook"),
          function(object, invis = FALSE, ...) {
              x = .combine.size(object, Inf)
              ob.names = object@ob.names
              by.type = split(x, x[[ob.names[3]]])

			  tmp.ask = by.type[[ob.names[6]]]
			  if(max(0, nrow(tmp.ask)) > 0){

                  tmp.ask = tmp.ask[order(tmp.ask[[ob.names[1]]]),]
			  }
			  tmp.bid = by.type[[ob.names[7]]]
			  if (max(0, nrow(tmp.bid)) > 0){
              	  tmp.bid = tmp.bid[order(tmp.bid[[ob.names[1]]], decreasing = TRUE),]
			  }

              if(invis == FALSE){
                  cat("Top Bid:           ",
                      .prettify(tmp.bid[[ob.names[1]]][1]), "\n")
                  cat("Size:              ",
                      .prettify(tmp.bid[[ob.names[2]]][1]), "\n \n")
                  cat("Top Ask:           ",
                      .prettify(tmp.ask[[ob.names[1]]][1]), "\n")
                  cat("Size:              ",
                      .prettify(tmp.ask[[ob.names[2]]][1]), "\n")
              }
              invisible(rbind(tmp.ask[1,], tmp.bid[1,]))
          }
          )

## Returns the spread.

setMethod("spread",
          signature(object = "orderbook"),
          function(object, ...) {
              x = object@current.ob
              ob.names = object@ob.names

              by.type = split(x, x[[ob.names[3]]])

              tmp.ask = by.type[[ob.names[6]]]
              tmp.bid = by.type[[ob.names[7]]]

              if(nrow(tmp.ask) == 0 | nrow(tmp.bid) == 0){
                  return(0)
              } else {


                  tmp.ask = min(tmp.ask[[ob.names[1]]])

                  tmp.bid = max(tmp.bid[[ob.names[1]]])


                  return(tmp.ask -  tmp.bid)
              }

          }
          )

## Returns a snapshot of the orderbook at a point in time. Does not work yet.

setMethod("snapshot",
          signature(object = "orderbook"),
          function(object, new.time, show = TRUE, ...){
              ob.names = object@ob.names
              x = orderbook(object@ob.data,
              price = ob.names[1],
              size = ob.names[2],
              type = ob.names[3],
              time = ob.names[4],
              id = ob.names[5],
              ask = ob.names[6],
              bid = ob.names[7],
              end = new.time)
              if(show == TRUE){
                  display(x, short = TRUE)
              }
              invisible(x)
          }
          )

## Remove an order by ID.

setMethod("remove.order",
          signature(object = "orderbook"),
          function(object, id, ...){

              x = object@current.ob
              ob.names = object@ob.names
              x = x[x[[ob.names[5]]] != id,]
              object@current.ob <- x
              invisible(object)

          }
          )

## Jump to trade index. Need to read in the entire orderbook first though
## to build trade.data.

setMethod("view.trade",
          signature(object = "orderbook"),
          function(object, n = 1, ...){
              x = object@trade.data
              y = object@trade.index

              currentindex = object@feed.index
              nextindex = sort(as.numeric(names(x)))[n]

              n = nextindex - currentindex

              invisible(read.orders(object, n))
          }
          )


## Reset to beginning.

setMethod("reset",
          signature(object = "orderbook"),
          function(object, ...){

              ob.names = object@ob.names
              current.ob = data.frame(NA, NA, NA, NA, NA)
              names(current.ob) = ob.names[1:5]

              invisible(new("orderbook",
                            current.ob = current.ob,
                            ob.names = ob.names,
                            feed = object@feed,
                            feed.index = 0,
                            ob.data = as.matrix(current.ob),
                            current.pos = 1,
                            trade.data = object@trade.data,
                            trade.index = 1))

          }
          )


