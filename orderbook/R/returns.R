## Return calculations. Need some test cases. Should probably be
## visible.

## Problems with this code:

## 1) The functions do too much. Each one ought to be broken up into
## separate parts.

## 2) Everything depends on input from an orderbook object. That is
## too hard for test cases and debugging. Instead, there should be two
## low level functions: midpoint.ret.df and twap.ret.df. Each of these
## takes a data frame (or two) as its inputs. It checks to make sure
## that those data frames have all the information they need. Then it
## calculates the appropriate return. (The df stands for data frame,
## but maybe that is a silly name.)

## The test cases for these functions should NOT use the same data
## that is in out standard sample, at least as a first pass. That data
## is too complex and confusing. Instead, you should construct an
## input file BY HAND that is very simple, sort of like the example
## that is used in the vignette. This would just have a handful of
## limit orders, cancellations and trades. But it will be maximally
## easy for anyone to see what is going on. Then, use this orderbook
## as input to calculate various returns.

## Ideas: Both functions should be able to take input that consists of
## only a singe trade or two. That makes test cases easy. But, they
## also need to be handle hundreds of trades (probably passed in as a
## data frame) so that we can use them in a production
## setting. (Although keep in mind that, relative to the difficult
## computational tasks in orderbook, these calculations are trivial.)

## So, both functions take a data frame of trades. But they also need
## to take a data frame of prices (or at least the mid point return
## does). We need to think about where these returns come from and how
## they are formatted. Again, we don't want to pass in an entire
## orderbook. In fact, we will need other functions which do take an
## orderbook object and, from that object, create the set of
## information that is passed to the return calculation functions.

## We need to worry about corner cases. (And we need to have test
## cases which cover corner cases. For example, what happens if the
## data we have ends at 4:00 PM, and we have a trade that occurs at
## 3:59, and then we want to know the five minite mid.point return for
## that trade? There is no right answer to this question, but the
## functions need to discuss this explicitly and provide at least two
## options for how to deal with this: First, return NA for any return
## calculation which uses data we do not have. Second (and probably
## the default) to provide a return for as much data as we have (in
## this case, giving back the 1 minute return even thought the user
## asks for the 5 minute return.

## So: We have function(s) that take an orderbook object and return
## the data that twap and midpoint returns need. We have functions
## that take this data and calculate returns. Then we have larger
## "wrapper" functions (similar to what we have now) that take an
## orderbook object, use these helper functions, and return an otrder
## book object with new data in the my.trades slot (if that is where
## this sort of stuff ought to go).

## Another topic. We need summary functions that use trade data to
## tell us more about an orderbook. How much trading was done? How
## profitable was that? This sort of information might also go in
## summary(), at least when the orderbook is of type trader.

## Note how "return" is calculated here. Not the normal way, but in
## pennies. In other words, a one penny move in a $10 stock makes us
## as much money as a one penny move in a $40 stock even though the
## return in the second is much less. In HFT land, the equity you
## needed to take the position is (almost) irrelevant. Anyway, we want
## our return functions to have options for working both ways.


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
