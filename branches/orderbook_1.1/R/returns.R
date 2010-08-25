
                                        #Needs to be a visible
                                        #function with a test case.

.midpoint.return <- function(object, trdprice, trdrow, trdtime, time){

    ## Midpoint Return, automatically finds the midpoint return for the
    ## selected message row number for a vector of time in seconds,
    ## e.g. c(5, 10, 60, 120) means find the midpoint return for 5s, 10s,
    ## 1 min, 2 min after the trade.


    ## Time is in seconds so multiply it to find milliseconds

    time <- time * 1000

    ## Pull out current time and add it to time

    time <- trdtime + time

    ## Find rows for time

    n <- .get.time.row(object@file, time)

    ## Append n to the trade row

    n <- append(trdrow, n)

    ## Get the order book at the rows desired.

    x <- .read.orders.multiple(object, n)

    ## Get the midpoint when the trade happens

    object@current.ob <- x[[1]]

    startmidpt <- mid.point(object)

    ## Loop through and calculate midpoints at the other times

    midpoints <- vector()

    for(i in 2:length(n)){

        object@current.ob <- x[[i]]
        midpoints[i - 1] <- mid.point(object)
    }

    ## If startmidpt > trdprice then it was a buy, otherwise a
    ## short. Calculate returns accordingly.

    if(startmidpt > trdprice)
        return(list(round(midpoints - trdprice, 3), startmidpt))
    else
        return(list(round(trdprice - midpoints, 3), startmidpt))
}


.twap.return <- function(object, trdprice, trdtime, time, startmidpt){

    ## Trade weighted average price for the vector of times given the
    ## order number and a vector of times, just as in
    ## .midpoint.return. Should these be one function?


    trade.data <- object@trade.data

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
