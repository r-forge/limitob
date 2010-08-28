## What should be done with these two methods? Combine them into one?
## Turn them into functions? I don't think they should be hidden. Need
## some test cases. Note that read.messages makes use of some C
## code. Note that read.time uses read.messages under the hood.

setMethod("read.messages",
          signature(object = "orderbook"),
          function(object, n){

              ## The following function reads the next n messages from the file from
              ## the current location within the orderbook (file.index). Use
              ## negative n to read previous messages. For example, typing
              ## read.messages(object, 100) would read 100 rows of the input file.

              stopifnot(is.numeric(n))

              if(identical(n, 0)){

                  ## If user types in 0 for n, do nothing and return
                  ## object.

                  return(object)

              } else if(n > 0){

                  ## If reading in a positive numver of orders, call
                  ## the C routine using .read.orders.c. (Should
                  ## probably change the name of this C code to
                  ## .read.messages.c.

                  invisible(.read.messages.c(object, object@file.index + n))

              } else if(n < 0){

                  ## Then reset the object and read in to
                  ## object@file.index + n rows. Since n is negative
                  ## we can add.

                  n <- object@file.index + n
                  object <- reset(object)
                  invisible(.read.messages.c(object, n))

              }

          }
          )

setMethod("read.time",
           signature(object = "orderbook"),
           function(object, time){

               ## Reads orders from the file until the time specified. For example,
               ## read.time(object, "9:30:00") returns the order book at 9:30:00.

               ## .get.time.row will return the row number of the
               ## first message with time greater than or equal to
               ## n. .to.ms converts n to milliseconds after midnight.

               n <- .get.time.row(object@file, .to.ms(time))

               ## Reset the object.

               object <- reset(object)

               ## Use read.messages to get to row n.

               invisible(read.messages(object, n))

          }
          )



