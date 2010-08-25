## Need to think hard about the three different choices we have for
## functions: hidden, normal and methods. I *think* that methods
## should only be used for true generics, like summary(), show() and
## so on. I *think* that hiddens should only be used for functions
## that the user should never call directly. Everything else should
## just be a normal function.

.plot.ob <-function(object, bounds){

    ## Plots the orderbook object at current time. Displays Bids on
    ## the left and Asks on the right with Price and Size on the Y-
    ## and X-axes, respectively.  Only prices within 10% above and
    ## below the midpoint value are shown.


    ## Use combine size to find the total size at each price level. This
    ## function returns a data frame. Also get the names for the columns.

    x <- .combine.size(object, bounds)

    ## If there is nothing on the orderbook, stop
    stopifnot(nrow(x)>0)

    ## Maximum size, max/min price and difference between the max
    ## and min price for purposes of drawing the axes.

    max.size <- max(x[["size"]])

    min.price <- min(x[["price"]])
    max.price <- max(x[["price"]])
    midpoint <- mid.point(object)

    ## Find best bid and best ask

    bestbid <- best.bid(object)[[1]]
    bestask <- best.ask(object)[[1]]

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

    tmp <- xyplot(price~size|type, data = x,

                  ylab = "Price", xlab = "Size (Shares)",

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

## Plot top ask vs top bid, 2nd best ask vs 2nd best bid, etc.

.plot.side.ob <-function(object, n){

    ## Aggregate the size at each price level

    x <- .combine.size(object, 1)

    ## Find the midpoint

    midpoint = mid.point(object)

    ## Creating the data frame to be plotted.

    ## Create an ask data frame. We want best ask at the top of the
    ## plot, and ask is already sorted ascending by price, so we just
    ## take the first 1 to k, where k is the smaller of n (user
    ## specified) or the number of rows in ask (we don't want errors).

    ask <- x[x[["type"]] == "ASK",]
    k <- min(n, nrow(ask))
    ask <- ask[1:k,]

    ## Create a bid data frame, we take bids from the bottom and up, since it is also sorted in ascending order.

    bid <- x[x[["type"]] == "BID",]
    k <- min(n, nrow(bid))
    bid <- bid[(nrow(bid) - k + 1):nrow(bid),]

    ## Rbind them together.

    x <- rbind(ask, bid)

    ## The ask and bid data frames may not have all price levels
    ## represented, e.g. there could be no shares for sale at 25.67,
    ## but shares at 25.66 and 25.68. This creates an entry for all
    ## price levels between the max and min.

    y <- data.frame(price = c(seq(ask[1,1], ask[1,1] + (n-1)/100, .01), # Better way to do this?
                    seq(bid[nrow(bid),1],
                        bid[nrow(bid),1] - (n-1)/100, -.01)),
                    y = c(seq(n, 1, -1)))

    ## Remove numerical inaccuracies.

    y$price <- round(y$price, 2)

    ## Merge them.

    x <- merge(x, y, all.y = TRUE)

    ## Setting x-axis limits and labels.

    max.size <- ceiling(max(x[["size"]], na.rm = TRUE))

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

    tmp <- barchart(y ~ size, data = x, groups = x$type, auto.key = TRUE,
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

    ## Draw the second axis label.

    trellis.focus("panel", 1, 1, clip.off = TRUE, highlight = FALSE)
    grid.text("Ask Price Levels", x = 1.14, rot = 90)
    trellis.unfocus()

}

                                        # ARRRRGGGGHHHHHH!

.plot.orders.ob <-function(object, bounds){

    ## Same as plot.ob, except # of orders instead of shares at each
    ## price level.

    x <- object@current.ob

    ## We only want data within our bounds

    x <- x[(x[["price"]] < mid.point(object)*(1+bounds)
            & x[["price"]] > mid.point(object)*(1-bounds)),]

    ## Create data.frame with price level, number of orders, and type

    ask <- x[x[["type"]] == "ASK",]
    bid <- x[x[["type"]] == "BID",]

    ask <- data.frame(table(ask[["price"]]))
    bid <- data.frame(table(bid[["price"]]))

    ask <- cbind(ask, rep("ASK", nrow(ask)))
    bid <- cbind(bid, rep("BID", nrow(bid)))

    names(ask) <- c("price", "Orders", "type")
    names(bid) <- names(ask)

    x <- rbind(ask, bid)
    x[["price"]] <- as.numeric(levels(x[["price"]]))

    ## Maximum orders, max/min price. and difference between the max
    ## and min price for purposes of drawing the axes.

    max.orders <- ceiling(max(x[["Orders"]]))

    min.price <- min(x[["price"]])
    max.price <- max(x[["price"]])
    midpoint <- mid.point(object)

    bestbid <- best.bid(object)[[1]]
    bestask <- best.ask(object)[[1]]

    ## Create x axes/limits.

    x.at <- pretty(c(0, max.orders))
    x.limits <- list(c(x.at[length(x.at)], 0),
                     c(0, x.at[length(x.at)]))

    ## Creating the y axis values.

    tmp.at <- c(pretty(c(min.price, max.price), n = 10), bestbid, bestask)
    tmp.at <- sort(tmp.at)

    yask.at <- tmp.at[tmp.at > midpoint]
    ybid.at <- tmp.at[tmp.at < midpoint]

    ## Remove y-axis labels if they are too close to the best bid or
    ## best ask

    space = (max.price - min.price)/20

    if(yask.at[1] + space > yask.at[2])
        yask.at = yask.at[-2]

    if(ybid.at[length(ybid.at)] - space < ybid.at[length(ybid.at) - 1])
        ybid.at = ybid.at[-(length(ybid.at) - 1)]

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

    ## Ordering the levels so Bid comes before Ask.

    x[["type"]] <- ordered(x[["type"]],
                           levels = c("BID", "ASK"))

    ## Actually plotting it.

    tmp <- xyplot(x[["price"]]~x[["Orders"]]|x[["type"]], data = x,

                  ylab = "Price", xlab = "Number of Orders",

                  main = paste("Order Book",
                  .to.time(object@current.time), sep = " - "),

                  scales = list(x = list(relation = "free",
                                limits = x.limits,
                                at = x.at,
                                axs = "i"),
                  y = list(alternating = 3)),

                  yscale.components = new.yscale.components,

                  panel = function(...){
                      panel.xyplot(...)
                      panel.lines(..., type = "H")
                  }
                  )

    ## Return the Trellis object

    invisible(tmp)

}



.animate.plot <- function(x, x.at, x.limits, y.limits, time, sub){

    ## This function is for plotting the Trellis objects used for
    ## animation. Connection to methods?

    ## Creating the data to be plotted. Basically take out the data
    ## inbetween the two limits, adding and subtracting .001 because
    ## there is numerical fuzziness.

    ask <- x[x[["type"]] == "ASK",]
    ask <- ask[ask$price < y.limits[2] + .001,]

    bid <- x[x[["type"]] == "BID",]
    bid <- bid[bid$price > y.limits[1] - .001,]

    x <- do.call(rbind, list(ask, bid))

    ## Order x by price

    x <- x[order(x$price),]


    ## Create a sequence of numbers from one y limit to the
    ## other. Round because of numerical fuzziness.

    price <- round(seq(y.limits[1], y.limits[2], .01), 2) # Better
                                                          # way? Built
                                                          # ins?
                                                          # ggplot2?

    ## Turn it into a column and merge it with x. This ensures that
    ## every price level in x has something within it.

    price <- cbind(price)
    x <- merge(x, price, all.y = TRUE)

    ## Order price and time

    x$price <- as.ordered(x$price)
    x$time <- as.ordered(x$time)

    ## Ordering the levels so Bid comes before Ask.

    x[["type"]] <- ordered(x[["type"]],
                           levels = c("BID", "ASK"))

    ## Creating the y-axis

    ymin = y.limits[1] - .01
    ybid.at <- (100 * (min(bid$price) - ymin)):(100 * (max(bid$price) - ymin))
    ybid.at <- round(ybid.at)
    yask.at <- (100 * (min(ask$price) - ymin)):(100 * (max(ask$price) - ymin))
    yask.at <- round(yask.at)

    new.yscale.components <- function(...) {
        ans <- yscale.components.default(...)
        ans$right <- ans$left

        ans$left$ticks$at <- ybid.at
        ans$left$labels$at <- ybid.at
        ans$left$labels$labels <- formatC(price[ybid.at], format =
                                          "f", digits = 2)

        ans$right$ticks$at <- yask.at
        ans$right$labels$at <- yask.at
        ans$right$labels$labels <- formatC(price[yask.at], format =
                                           "f", digits = 2)

        ans
    }

    ## Actually plotting it.

                                        # Comments, please!

    tmp <- barchart(price ~ size | type, data = x,

                    ylab = "Price", xlab = "Size (Shares)",
                    groups = interaction(x$status, x$time),
                    main = paste("Order Book", time, sep = " -- "),
                    stack = TRUE, sub = sub,

                    col = c("gray", "gray50", "blue", "red", "green"),


                    border = "transparent",
                    scale = list(x = list(relation = "free", at = x.at,
                                 limits = x.limits, axs = "i", rot = 45),
                    y = list(alternating = 3)),
                    yscale.components = new.yscale.components
                    )

    invisible(tmp)
}

                                        # Separate file.

.supply.demand.plot <- function(object, bounds){

    ## Plots normalized supply and demand for the order book following
    ## Cao.

    ## Find the size at each price level using .combine.size.

    x <- .combine.size(object, bounds)

    ## Pull out all the asks.

    ask <- x[x$type == "ASK",]

    ## Pull out all the bids and sort the price by decreasing order.

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

    ask$size <- cumsum(ask$size)/sum(ask$size)
    bid$size <- cumsum(bid$size)/sum(bid$size)

    ## Rbind it all together.

    x <- rbind(ask, bid)

    ## Add two rows each with 0 size to ensure that the graph starts
    ## plotting at 0.

    x <- rbind(x, c(ask$price[1], 0, "ASK"), c(bid$price[1], 0, "BID"))

    ## Cast size and price as numeric.

    x$size <- as.numeric(x$size)
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

    tmp <- xyplot(x$price ~ x$size, data = x, groups = x$type, type =
                  "S", ylab = "Price (%)",
                  xlab = "Size (%)", main = "Supply and Demand", sub =
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
