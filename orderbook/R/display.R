setMethod("display",
          signature(object = "orderbook"),
          function(object, n = 5, ...){

              ## Displays the price levels and sizes. n specifies the number of rows to be
              ## displayed for ask and bid.

              ## Combine size returns a data frame with the size
              ## aggregated for each price level. Output data frame is
              ## sorted.

              x <- agg.price.levels(object, bounds = Inf, kind = "shares")

              ## Create ask and bid data frames. Keep rows you
              ## want. Assumes that agg.price.levels gives them to you
              ## nicely sorted.

              ask <- tail(x[x$type == "ASK",], n = n)
              bid <- head(x[x$type == "BID",], n = n)

              ## Print out current time.

              cat("\nCurrent time is",
                  .to.time(object@current.time), "\n\n")
              cat("\t\t Price \t Ask Size\n")
              cat("---------------------------------------------\n")

              ## Print out the top n ask prices/sizes

              for(i in 1:nrow(ask)){
                  cat("\t\t",
                      .prettify(ask[["price"]][i]), "\t",
                      .prettify(ask[["shares"]][i], "s"), "\n")
              }
              cat("---------------------------------------------\n")

              ## Print out the top n bid prices/sizes

              for(i in 1:nrow(bid)){

                  ## Spacing for right alignment, max size is
                  ## 1 million for this to work

                  size = .prettify(bid[["shares"]][i], "s")

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

