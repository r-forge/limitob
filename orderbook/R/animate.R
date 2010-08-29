## To do: Delete duplicated code. Combine these together is a sensible
## way.

animate <- function(object,
                    by = "sec",
                    start = NULL,
                    end = NULL,
                    pause = 0.25,
                    initPause = 2){

    ## Animate is to be used in conjunction with
    ## load.trade.animation or load.animation. After an
    ## animation is created its location is stored in
    ## object@animation. This function loads the animation
    ## and uses a for loop to play through it.

    ## Load the trade animation stored in object@animation
    ## according to type.

    filename <- object@animation[[by]]
    load(filename)

    ## Remove "name" slot from the name vector.

    name <- name[-length(name)]

    ## Initial pause

    Sys.sleep(initPause)

    ## If start is null, then make it 1, otherwise its the
    ## middle of name - start

    if(is.null(start))
        start <- 1
    else
        start <- ceiling(length(name)/2) - start

    ## If end is null, then make it the length of name,
    ## otherwise its the middle of name + start

    if(is.null(end))
        end <- length(name)
    else
        end <- floor(length(name)/2) + end

    ## Loop through name to print all the objects.

    for(i in start:end){

        print(get(name[i]))

        ## Pause during each plot.

        Sys.sleep(pause)
    }

}

load.animation <- function(object,
                           from,
                           to,
                           fps = 1,
                           by = "sec",
                           bounds = 0.02){

    ## Create Trellis objects that are used for the animation and save
    ## them using tempfile(). Put the location of tempfile() in
    ## orderbook@animation. Can view by message or seconds.

    if(isTRUE(by %in% "sec")){

        time <- seq(.to.ms(from), .to.ms(to), 1000/fps)

        ## Run helper function to do the actual creation of
        ## the Trellis objects.

        invisible(.animate.seconds(object, time, bounds))

    } else if(isTRUE(by %in% "msg")){

        ## Run helper function to do actual creation of the
        ## Trellis objects.

        invisible(.animate.orders(object, seq(from, to),
                                  bounds))

    }
}



.load.trade.animation <- function(object, tradenum, before = 30, after = 30, fps = 1, by =
                                   "both", bounds = 0.02){

              ## Load trade animation given a trade number.

              ## Extract the desired trade from my trades.

              trade <- object@my.trades[tradenum,]

              if(isTRUE(by %in% "sec") | isTRUE(by %in% "both")){

                  ## Find the time 30 seconds before and 30 seconds
                  ## (30 seconds is the default) after that time.

                  ## Obtain the time by pulling the 2nd column.

                  tradetime <- trade[[2]]

                  ## Convert to an actual time format.

                  tradetime <- .to.time(tradetime)

                  ## Create a POSIX object.

                  tradetime <- as.POSIXlt(tradetime, format = "%H:%M:%S")

                  ## Establish the range of time to observe in the animation.

                  from <- format(tradetime - before, format = "%H:%M:%S")
                  to <- format(tradetime + after, format = "%H:%M:%S")

                  time <- seq(.to.ms(from), .to.ms(to), 1000/fps)

                  object <- .animate.seconds(object, time, bounds, trade)
              }

              if(isTRUE(by %in% "msg") | isTRUE(by %in% "both")){

                  ## Obtain the row by pulling the first column.

                  traderow <- trade[[1]]

                  ## Animate the orderbook from n to before + after messages after n.

                  object <- .animate.orders(object, seq(traderow -
                                                        before,
                                                        traderow +
                                                        after),
                                            bounds, trade)

              }

              ## Set a new trade.index and return the object.

              object@trade.index <- tradenum
              invisible(object)

          }


## If these are only used by animation, then the names are bad. Also,
## aren't they almost identical?

load.next.trade <- function(object, before = 30, after = 30, fps = 1, by =
                             "both", bounds = .02){

    ## Loads trade animation for the current trade index then increments
    ## it by 1.

    object <- .load.trade.animation(object,
                                    object@trade.index,
                                    before, after, fps, by,
                                    bounds)

    object@trade.index <- object@trade.index + 1

    invisible(object)
}



load.previous.trade <- function(object, before = 30, after = 30, fps = 1, by =
                                 "both", bounds = .02){

    ## Loads trade animation for current trade index - 1 then decrements
    ## it by 1.

    if(object@trade.index > 1){

        object <- .load.trade.animation(object,
                                        object@trade.index - 1,
                                        before, after, fps, by,
                                        bounds)

        object@trade.index <- object@trade.index - 1

    }

    invisible(object)

}



## We need to combine .animate.seconds and .animate.orders (which
## ought to be called .animate.messages). Perhaps .animate would work
## for the combined, with a type argument. Key differences seem to
## include the very start and the subtitles section.

.animate.seconds <- function(object, time, bounds, trade = NULL){

    ## Create a list that will store the current orderbooks for each
    ## time, as well as the variables that hold the y and x
    ## limits. sub is for the subtitles.

    ## How long does this take? Would lapply help?

    n <- .get.time.row(object@file, time)

    current.ob = .read.messages.multiple(object, n)
    y.limits = c(Inf, 0)
    max.size = 0

    if(is.null(trade))
        sub <- ""
    else
        sub <- paste("Start:", .to.time(time[1]), " Trade:",
                     .to.time(trade[[2]]), " End:", .to.time(time[length(time)]))

    ## Use a for loop to create all the current.ob and take the
    ## smallest/biggest axes.

    for(i in 1:length(n)){

        ## Generate the object for the next time, put the current.ob
        ## into our list, and put "" in the subtitle (no subtitles
        ## until slow). (Until slow?)

        object@current.ob <- current.ob[[i]]

        x <- agg.price.levels(object, bounds = 1, kind = "shares")
        mid <- mid.point(object)

        ## Find the min ask and max bid price for this current.ob

        ask <- x[x[["type"]] == "ASK",]
        ask <- ask[ask$price < min(ask$price) + bounds,]

        bid <- x[x[["type"]] == "BID",]
        bid <- bid[bid$price > max(bid$price) - bounds,]

        ## Check to see if the y limits are bigger/smaller than the
        ## existing ones.

        if(min(bid$price) < y.limits[1])
            y.limits[1] <- min(bid$price)

        if(max(ask$price) > y.limits[2])
            y.limits[2] <- max(ask$price)

        ## Find the max size for this current.ob

        tmp.max.size <- max(x$shares[x$price <= y.limits[2] & x$price >=
                                   y.limits[1]])

        ## Check to see if the x limits are bigger/smaller than the
        ## existing ones.

        if(tmp.max.size > max.size)
            max.size <- tmp.max.size

    }

    ## Creating the x limits and tick locations

    x.at <- pretty(c(0, max.size))
    x.limits <- list(c(x.at[length(x.at)], 0),
                     c(0, x.at[length(x.at)]))



    ## Use a for-loop to create all the Trellis objects. Create a name
    ## vector.

    name <- vector()

    for (i in 1:length(n)){

                                        # Lousy function name.

        tmp.plot <- .animate.plot(current.ob[[i]], x.at, x.limits, y.limits,
                                  .to.time(time[i]), sub)

        name[i] <- paste("y", i, sep = ".")
        assign(paste("y", i, sep = "."), tmp.plot)

    }

    ## Save the names vector

    name[length(name) + 1] = "name"
    assign("name", name)


    ## Save the Trellis objects.

    tempfile <- tempfile()
    otherfile <- object@animation[["msg"]]

    object@animation <- list(sec = tempfile, msg = otherfile)

    save(list = name, file = tempfile)

                                        # What are we returning and why?

    invisible(object)

}



.animate.orders <- function(object, n, bounds, trade = NULL){

    ## Create a list that will store the current orderbooks for each
    ## time, as well as the variables that hold the y and x
    ## limits. sub is for the subtitles.

    current.ob <- .read.messages.multiple(object, as.integer(n))

    time <- vector()
    y.limits <- c(Inf, 0)
    max.size <- 0

    ## Use a for loop to create all the current.ob and take the
    ## smallest/biggest axes.

    for(i in 1:length(n)){

        object@current.ob <- current.ob[[i]]

        time[i] <- max(current.ob[[i]]$time)

        x <- agg.price.levels(object, bounds = 1, kind = "shares")
        mid <- mid.point(object)

        ## Find the min ask and max bid price for this current.ob

        ask <- x[x[["type"]] == "ASK",]
        ask <- ask[ask$price < min(ask$price) + bounds,]

        bid <- x[x[["type"]] == "BID",]
        bid <- bid[bid$price > max(bid$price) - bounds,]

        ## Check to see if the y limits are bigger/smaller than the
        ## existing ones.

        if(min(bid$price) < y.limits[1])
            y.limits[1] <- min(bid$price)

        if(max(ask$price) > y.limits[2])
            y.limits[2] <- max(ask$price)

        ## Find the max size for this current.ob

        tmp.max.size <- max(x$shares[x$price < y.limits[2] & x$price >
                                     y.limits[1]])

        ## Check to see if the x limits are bigger/smaller than the
        ## existing ones.

        if(tmp.max.size > max.size)
            max.size <- tmp.max.size

    }

    ## Creating the x limits and tick locations

    x.at <- pretty(c(0, max.size))
    x.limits <- list(c(x.at[length(x.at)], 0),
                     c(0, x.at[length(x.at)]))

    ## Create subtitles

    if(is.null(trade))
        sub <- ""
    else
        sub <- paste("Start:", .to.time(time[1]), " Trade:",
                     .to.time(trade[[2]]), " End:", .to.time(time[length(time)]))


    ## Use a for-loop to create all the Trellis objects and create
    ## name vector.

    name = vector()

    for (i in 1:length(n)){


        tmp.plot <- .animate.plot(current.ob[[i]], x.at, x.limits, y.limits,
                                  .to.time(time[i]), sub)

        name[i] <- paste("y", i, sep = ".")
        assign(paste("y", i, sep = "."), tmp.plot)

    }

    ## Save the names vector

    name[length(name) + 1] = "name"
    assign("name", name)


    ## Save the Trellis objects.

    tempfile <- tempfile()
    otherfile <- object@animation[["sec"]]

    object@animation <- list(sec = otherfile, msg = tempfile)

    save(list = name, file = tempfile)

    invisible(object)

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
