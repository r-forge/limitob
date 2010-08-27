## Should inside.market really be a method? I don't see anything wrong
## with having all these other functions use it.

setMethod("inside.market",
          signature(object = "orderbook"),
          function(object, invis = FALSE, ...){

              ## Returns a data frame with a row for the best ask and a row for the
              ## best bid.  The columns are price, size, and type.

              ask <- best(object, side = "ASK")
              bid <- best(object, side = "BID")

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

best <- function(x, side){

    ## Returns a named vector with the price and size of the best bid
    ## or ask in orderbook x. Perhaps ought to return this data in
    ## some other fashion?

    side <- toupper(side)
    stopifnot(side %in% c("ASK", "BID"))
    stopifnot(class(x) == "orderbook")


    ## Pull out current.ob into x.

    x <- x@current.ob

    ## Takes out the correct side

    x <- x[x[["type"]] == side,]

    if(nrow(x) == 0){

        return(NA)

    } else {

        ## Gets indices for the best bid or ask. Is there some cooler
        ## way of doing this? Only difference is max for bid and min
        ## for ask.

        if(side %in% "BID"){
            index <- which(x[["price"]] == max(x[["price"]]))
        }
        else{
            index <- which(x[["price"]] == min(x[["price"]]))
        }

        ## Any of the indices would get us the best price, so we
        ## just use the first one. Better way?

        price <- x[["price"]][index[1]]

        ## Sum to find the total size at the indices.

        size <- sum(x[["size"]][index])

        ## Return named vector of the best bid/ask price and
        ## total size at that price level.

        return(c(price = price, size = size))
    }

}






setMethod("mid.point",
          signature(object = "orderbook"),
          function(object, ...) {

              ## Returns the midpoint value, which is just the simple average of the
              ## best bid and ask.

              return((best(object, side = "BID")[1] + best(object, side = "ASK")[1])/2)

          }
          )



setMethod("spread",
          signature(object = "orderbook"),
          function(object, ...){

              ## Returns the spread.
                                        # Combine with midpoint?
                                        # Test cases?

              ## Gets best bid and best ask.

              ask = best(object, side = "ASK")
              bid = best(object, side = "BID")

              ## If there are either no bids or no asks, then return
              ## NA. Otherwise return the difference of the best ask
              ## and best bid. Need an NA test case. Ought to be a
              ## cleaner way to code this.

              if(is.na(ask) || is.na(bid)){

                  return(NA)

              } else{

                  return(ask[["price"]] - bid[["price"]])

              }

          }
          )
