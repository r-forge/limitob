## Should inside.market really be a method? I don't see anything wrong
## with having all these other functions use it.

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
                                        # Seems horribly redundant!
                                        # Just give inside.market an
                                        # argument which allows for
                                        # "bid", "ask" or "both."

best.bid <- function(object, ...){

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



best.ask <- function(object, ...){

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




setMethod("mid.point",
          signature(object = "orderbook"),
          function(object, ...) {

              ## Returns the midpoint value, which is just the simple average of the
              ## best bid and ask.

              return((best.bid(object)[1] + best.ask(object)[1])/2)

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
