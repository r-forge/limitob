## What should be done with these two methods? Combine them into one?
## Turn them into functions? I don't think they should be hidden. Need
## some test cases. Note that read.orders (which really should be
## called read.messages) makes use of some C code. Note that read.time
## used read.orders under the hood.

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



