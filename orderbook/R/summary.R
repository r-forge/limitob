setMethod("summary",
          signature(object = "orderbook"),
          function(object){

              ## Displays summary information.

              cat("\nCurrent time is",
                  .to.time(object@current.time), "\n\n")
              cat("Ask price levels:  ",
                  .prettify(o.and.l(ob, "ask", "levels"), "s"), "\n")
              cat("Bid price levels:  ",
                  .prettify(o.and.l(ob, "bid", "levels"), "s"), "\n")
              cat("Total price levels:",
                  .prettify(o.and.l(ob, "all", "levels"), "s"), "\n")
              cat("-----------------------------\n")
              cat("Ask orders:        ",
                  .prettify(o.and.l(ob, "ask", "orders"), "s"), "\n")
              cat("Bid orders:        ",
                  .prettify(o.and.l(ob, "bid", "orders"), "s"), "\n")
              cat("Total orders:      ",
                  .prettify(o.and.l(ob, "all", "orders"), "s"), "\n")
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

