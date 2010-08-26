setMethod("show",
          signature(object = "orderbook"),
          function(object){

              ## Show basic information about the order book at its current
              ## state. .prettify is a helper function to make the output print
              ## nicely.

              cat("An object of class orderbook\n")
              cat("--------------------------\n")
              cat("Current orderbook time:   ",
                  .to.time(object@current.time), "\n")
              cat("Message Index:            ",
                  .prettify(object@file.index, "s"), "\n")
              cat("Bid Orders:               ",
                  .prettify(o.and.l(object, "bid", "orders"), "s"), "\n")
              cat("Ask Orders:               ",
                  .prettify(o.and.l(object, "ask", "orders"), "s"), "\n")
              cat("Total Orders:             ",
                  .prettify(o.and.l(object, "all", "orders"), "s"), "\n")
          }
          )
