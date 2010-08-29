## Plotting is currently a mess. Get rid of the duplicate code. Split
## these up into separate files? Make the hidden plot functions
## visible to the user? How do I ensure that the help for plot is easy
## to access? Should plot() be a generic or just a regular function?

setMethod("plot",
          signature(x = "orderbook"),
          function(x, type = "n", bounds = 0.1, n = 10){

              ## Plot calls helper plot methods.

              stopifnot(type %in% c("n", "o", "sd", "s"))

              ## Check the type of plot the user wants then calls the
              ## helper function. Most plot helper functions return a
              ## Trellis object that we need to print.

              if(type %in% c("n", "o")){

                  ## Plots the size of the orders at each bid and ask
                  ## independently.

                  tmp <- .plot.ob(x, type = type, bounds = bounds)
                  print(tmp)

              } else if(type %in% "s"){

                  ## Plots the size of each level of bid and ask
                  ## together. ie. best bid and best ask are the same
                  ## level. Note the hack that this prints
                  ## automatically and then messes with the
                  ## plot. Ought to fix this.

                  .plot.side.ob(x, n)

              } else if(type %in% "sd"){

                  ## Graphs the normalized price against normalized
                  ## size to reflect a supply/demand curve.

                  tmp <- .supply.demand.plot(x, bounds)
                  print(tmp)

              }
          }
          )


.plot.ob <-function(object, type, bounds){

    ## Plots the orderbook object at current time. Displays Bids on
    ## the left and Asks on the right with Price and Size on the Y-
    ## and X-axes, respectively.  Only prices within bounds % above
    ## and below the midpoint value are shown. type can be either "n"
    ## or "o".

    stopifnot(type %in% c("n", "o"))

    if(type %in% c("o")){

        x <-  agg.price.levels(object, bounds = bounds, kind = "orders")
        var <- "orders"

    } else{

        ## Find the total size at each price level.

        x <- agg.price.levels(object, bounds = bounds, kind = "shares")
        var <- "shares"
    }

    ## If there is nothing on the orderbook, stop

    stopifnot(nrow(x)>0)


    ## Maximum size, max/min price and difference between the max
    ## and min price for purposes of drawing the axes.

    max.size <- max(x[[var]])

    min.price <- min(x[["price"]])
    max.price <- max(x[["price"]])
    midpoint <- mid.point(object)

    ## Find best bid and best ask

    bestbid <- best(object, side = "BID")[[1]]
    bestask <- best(object, side = "ASK")[[1]]

    ## Creating the x axis values.

    x.at <- pretty(c(0, max.size))
    x.limits <- list(c(x.at[length(x.at)], 0),
                     c(0, x.at[length(x.at)]))

    ## Creating the y axis values and appending the best ask/bestbid to them.

    tmp.at <- c(pretty(c(min.price, max.price), n = 10), bestbid, bestask)
    tmp.at <- sort(tmp.at)

    yask.at <- tmp.at[tmp.at > midpoint]
    ybid.at <- tmp.at[tmp.at < midpoint]


    ## Remove y-axis labels if its too close to the best bid or best
    ## ask.

    space = (max.price - min.price)/20

    if(yask.at[1] + space > yask.at[2])
        yask.at = yask.at[-2]

    if(ybid.at[length(ybid.at)] - space < ybid.at[length(ybid.at) - 1])
        ybid.at = ybid.at[-(length(ybid.at) - 1)]

    ## Function for drawing the y-axis.

    new.yscale.components <- function(...) {
        ans <- yscale.components.default(...)
        ans$right <- ans$left

        ans$left$ticks$at <- ybid.at
        ans$left$labels$at <- ybid.at
        ans$left$labels$labels <- formatC(ybid.at, format = "f",
                                          digits = 2)

        ans$right$ticks$at <- yask.at
        ans$right$labels$at <- yask.at
        ans$right$labels$labels <- formatC(yask.at, format = "f",
                                           digits = 2)
        ans
    }

    ## Ordering the levels so Bid comes before Ask, this allows Bid to be
    ## on the left side of the plot.

    x[["type"]] <- ordered(x[["type"]], levels = c("BID",
                                        "ASK"))

    ## Actually plotting it.

    ## x labels are (nicely) tilted when we have big numbers for
    ## shares, but only (unecessarily) titled when we have small
    ## numbers for orders. Did I cause this by getting rid of the
    ## .plot.orders.on() hack?

    tmp <- xyplot(price ~ get(var) | type, data = x,

                  ylab = "Price", xlab = var,

                  main = paste("Order Book",
                  .to.time(object@current.time), sep = " - "),

                  yscale.components = new.yscale.components,

                  scales = list(x = list(relation = "free",
                                limits = x.limits,
                                at = x.at,
                                axs = "i",
                                rot = 45),
                  y = list(alternating = 3)),
                  panel = function(...){
                      panel.xyplot(...)
                      panel.lines(..., type = "H")
                  }
                  )

    ## Return the Trellis object.

    invisible(tmp)

}



.plot.side.ob <-function(object, n){

    ## Plot top ask vs top bid, 2nd best ask vs 2nd best bid, etc. Not
    ## even sure that we care about this plot, although an animation using
    ## it might be interesting . . . allowing one to see more directly
    ## changes in orderbook balance.

    ## Maybe change the scheme so that the bids are below the asks.

    ## Aggregate the size at each price level

    x <- agg.price.levels(object, bounds = 1, kind = "shares")

    ## Find the midpoint

    midpoint = mid.point(object)

    ## Creating the data frame to be plotted.

    ## Create an ask data frame. We want best ask at the top of the
    ## plot, and ask is already sorted ascending by price, so we just
    ## take the first 1 to k, where k is the smaller of n (user
    ## specified) or the number of rows in ask (we don't want errors).

    ask <- tail(x[x$type == "ASK",], n = n)

    ## Create a bid data frame, we take bids from the bottom and up, since it is also sorted in ascending order.

    bid <- head(x[x$type == "BID",], n = n)

    ## Rbind them together.

    x <- rbind(ask, bid)

    ## The ask and bid data frames may not have all price levels
    ## represented, e.g. there could be no shares for sale at 25.67,
    ## but shares at 25.66 and 25.68. This creates an entry for all
    ## price levels between the max and min.

    y <- data.frame(price = c(seq(ask[nrow(ask), 1], ask[nrow(ask), 1] + (n - 1)/100, .01), # Better way to do this?
                    seq(bid[1, 1],
                        bid[1, 1] - (n - 1)/100, -.01)),
                    y = c(seq(n, 1, -1)))

    ## Remove numerical inaccuracies.

    y$price <- round(y$price, 2)

    ## Merge them.

    x <- merge(x, y, all.y = TRUE)

    ## Setting x-axis limits and labels.

    max.size <- ceiling(max(x$shares, na.rm = TRUE))

    x.at <- pretty(c(0, max.size))
    x.limits <- c(0, x.at[length(x.at)])

    ## Creating y-axis tick labels.

    yask.at <- rev(x$price[x$price > midpoint])
    ybid.at <- x$price[x$price < midpoint]

                                        # Utter hack to stick a
                                        # function in the middle here!
                                        # Especially since this exact
                                        # same function is repeated
                                        # later!

    new.yscale.components <- function(...) {
        ans <- yscale.components.default(...)
        ans$right <- ans$left

        ans$left$labels$at <- ybid.at
        ans$left$labels$labels <- formatC(ybid.at, format = "f",
                                          digits = 2)

        ans$right$labels$at <- yask.at
        ans$right$labels$labels <- formatC(yask.at, format = "f",
                                           digits = 2)

        ans
    }

    ## This is to help draw our second axis label (on the right side).

    new.par.settings = list(
    layout.widths = list(left.padding = 2, right.padding = 5))

    ## Actually plot it.

    tmp <- barchart(y ~ shares, data = x, groups = x$type, auto.key = TRUE,
                    ylab = "Bid Price Levels", xlab = "Size (Shares)",
                    main = "Order Book", par.settings = new.par.settings,
                    yscale.components = new.yscale.components,
                    scales = list(x = list(axs = "i",
                                  limits = x.limits,
                                  at = x.at,
                                  tck = c(1, 0)),
                    y = list(alternating = 3))
                    )

    plot(tmp)

    ## Draw the second axis label. Total hack to print this and then
    ## manipulate the plot! Very different from the other plotting
    ## helper functions which just create a plot and pass it back for
    ## printing.

    trellis.focus("panel", 1, 1, clip.off = TRUE, highlight = FALSE)
    grid.text("Ask Price Levels", x = 1.14, rot = 90)
    trellis.unfocus()

}



.supply.demand.plot <- function(object, bounds){

    ## Plots normalized supply and demand for the order book following
    ## Cao.

    ## Find the size at each price level.

    x <- agg.price.levels(object, bounds = bounds, kind = "shares")

    ## Pull out all the asks.

    ask <- x[x$type == "ASK",]
    ask <- ask[order(ask$price),]

    ## Pull out all the bids. Note the bids have been sorted correctly
    ## by agg.price.levels but we resort anyway.

    bid <- x[x$type == "BID",]
    bid <- bid[order(bid$price, decreasing = TRUE),]

    ## Normalize ask and bid prices by subtracting the midpoint and
    ## then dividing by the maximum ask/absolute value of the maximum
    ## bid

    ask$price <- ask$price - mid.point(object)
    bid$price <- bid$price - mid.point(object)

    ask$price <- ask$price/max(ask$price)
    bid$price <- bid$price/max(abs(bid$price))

    ## Normalize the sizes by dividing the cumulative size at each
    ## price level by the total size on the side.

    ask$shares <- cumsum(ask$shares)/sum(ask$shares)
    bid$shares <- cumsum(bid$shares)/sum(bid$shares)

    ## Rbind it all together.

    x <- rbind(ask, bid)

    ## Add two rows each with 0 size to ensure that the graph starts
    ## plotting at 0.

    x <- rbind(x, c(ask$price[1], 0, "ASK"), c(bid$price[1], 0, "BID"))

    ## Cast size and price as numeric.

    x$shares <- as.numeric(x$shares)
    x$price <- as.numeric(x$price)

    ## Define the x and y limits/labels. This is done according to
    ## Cao.

    x.limits = c(0, 1.2)
    x.at = c(0, .2, .4, .6, .8, 1, 1.2)
    x.labels = c(0, 20, 40, 60, 80, 100, 120)

    y.limits <- c(-1.5, 1.5)
    y.at = c(-1.5, -1, -.5, 0, .5, 1, 1.5)
    y.labels = c(-150, -100, -50, 0, 50, 100, 150)

    ## Actually plot and return.

    tmp <- xyplot(x$price ~ x$shares, data = x, groups = x$type, type =
                  "S", ylab = "Price (%)",
                  xlab = "Shares (%)", main = "Supply and Demand", sub =
                  .to.time(object@current.time),
                  scales = list(

                  x = list(limits = x.limits, axs = "i", at = x.at, labels =
                  x.labels, tck = c(1, 0)),

                  y = list(limits = y.limits, at = y.at, labels = y.labels, tck =
                  c(1, 0))),

                  panel = function(...){
                      panel.xyplot(...)
                      panel.abline(h = 0)
                  }
                  )
    invisible(tmp)
}
