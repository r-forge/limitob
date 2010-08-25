
                                        # Is .animate.messages
                                        # different?  Put all animate
                                        # stuff separately.

.animate.seconds <- function(object, time, bounds, trade = NULL){

    ## Create a list that will store the current orderbooks for each
    ## time, as well as the variables that hold the y and x
    ## limits. sub is for the subtitles.

    ## How long does this take? Would lapply help?

    n <- .get.time.row(object@file, time)

    current.ob = .read.orders.multiple(object, n)
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

        x <- .combine.size(object, 1)
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

        tmp.max.size <- max(x$size[x$price <= y.limits[2] & x$price >=
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

    ## Don't really understand what this does. Should it be named
    ## "animate.messages?" Ridiculous amount of code reuse as compared
    ## with .animate.seconds.

    ## Create a list that will store the current orderbooks for each
    ## time, as well as the variables that hold the y and x
    ## limits. sub is for the subtitles.

    current.ob <- .read.orders.multiple(object, as.integer(n))

    time <- vector()
    y.limits <- c(Inf, 0)
    max.size <- 0

    ## Use a for loop to create all the current.ob and take the
    ## smallest/biggest axes.

    for(i in 1:length(n)){

        object@current.ob <- current.ob[[i]]

        time[i] <- max(current.ob[[i]]$time)

        x <- .combine.size(object, 1)
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

        tmp.max.size <- max(x$size[x$price < y.limits[2] & x$price >
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
