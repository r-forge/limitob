## Return calculations. Need some test cases. Should probably be
## visible.

trade.returns <-function(x, time = c(5, 300)){

    ## Initialize Trades--calculate midpoint return/trade
    ## weighted average price return for all trades in
    ## my.trades. Takes the object as well as a vector of
    ## time in seconds to decide the times for finding the
    ## returns.

    ## Get my trades.

    mytrades <- x@my.trades

    ## Create a list to store the trade returns.

    tradereturns = list()

    ## For each of my trades

    for(i in 1:nrow(mytrades)){

        ## Find its row, price, and time within the data
        ## file

        trdrow <- mytrades$row[i]
        trdprice <- mytrades$price[i]
        trdtime <- mytrades$time[i]

        ## Given that information find the midpoint and twap

        midpt.ret <- midpoint.return(x, trdprice,
                                     trdrow, trdtime, time)

        tmp <- c(midpt.ret[[1]],
                 twap.return(x, trdprice, trdtime,
                             time, midpt.ret[[2]]))

        ## Put that vector into a list

        tradereturns[[i]] <- tmp
    }

    ## Turn the list into a matrix

    tmp <- do.call(rbind, tradereturns)

    ## Bind the returns with mytrades

    mytrades <- cbind(mytrades, tmp)

    ## Create a name vector and double the time vector (for
    ## midpoint and twap)

    names = vector()

    ## Create column names for midpoint

    for(i in 1:length(time)){
        names[i] = paste("midpoint", time[i], sep = ".")
    }

    ## Create column names for twap

    for(i in (length(time) + 1):(length(time) * 2)){
        names[i] = paste("twap", time[i - length(time)], sep
             = ".")
    }

    ## Put the names on the data frame

    names(mytrades) <- append(names(mytrades)[1:5], names)
    rownames(mytrades) <- NULL

    ## Put the new data frame into my trades

    x@my.trades <- mytrades

    ## Return the object

    return(x)
}




midpoint.return <- function(x, trdprice, trdrow, trdtime, time){

    ## Midpoint Return, automatically finds the midpoint return for the
    ## selected message row number for a vector of time in seconds,
    ## e.g. c(5, 10, 60, 120) means find the midpoint return for 5s, 10s,
    ## 1 min, 2 min after the trade.


    ## Time is in seconds so multiply it to find milliseconds

    time <- time * 1000

    ## Pull out current time and add it to time

    time <- trdtime + time

    ## Find rows for time

    n <- .get.time.row(x@file, time)

    ## Append n to the trade row

    n <- append(trdrow, n)

    ## Get the order book at the rows desired.

    y <- .read.messages.multiple(x, n)

    ## Get the midpoint when the trade happens

    x@current.ob <- y[[1]]

    startmidpt <- mid.point(x)

    ## Loop through and calculate midpoints at the other times

    midpoints <- vector()

    for(i in 2:length(n)){

        x@current.ob <- y[[i]]
        midpoints[i - 1] <- mid.point(x)
    }

    ## If startmidpt > trdprice then it was a buy, otherwise a
    ## short. Calculate returns accordingly.

    if(startmidpt > trdprice)
        return(list(round(midpoints - trdprice, 3), startmidpt))
    else
        return(list(round(trdprice - midpoints, 3), startmidpt))
}


twap.return <- function(x, trdprice, trdtime, time, startmidpt){

    ## Trade weighted average price for the vector of times given the
    ## order number and a vector of times, just as in
    ## .midpoint.return. Should these be one function?


    trade.data <- x@trade.data

    ## Time is in seconds so multiply it to find milliseconds

    time <- time * 1000

    ## Pull out current time and add it to time

    time <- trdtime + time

    twap <- vector()

    for(i in 1:length(time)){

        temp <- trade.data[trade.data$time >= trdtime &
                           trade.data$time <= time[i],]

        twap[i] = sum(temp$price * temp$size)/sum(temp$size)

    }

    ## If startmidpt > trdprice then it was a buy, otherwise a
    ## short. Calculate returns accordingly.

    if(startmidpt > trdprice)
        return(round(twap - trdprice, 3))
    else
        return(round(trdprice - twap, 3))

}
