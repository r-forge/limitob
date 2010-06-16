## Takes in object and number of lines to be read, 0 means read to end.

read.orders <- function(ob, n)
{

    ## Only run update if there are rows in current.ob.

    if(nrow(ob@current.ob) > 1){
        ob = .update(ob, n)
    }

    feed = ob@feed
    feed.index = ob@feed.index

    ob.data = ob@ob.data
    current.pos = ob@current.pos

    ids = ob@ids

    trade.data = ob@trade.data

    feed <- file(feed, open="r")


    x <- scan(feed, nline = 1, sep = ",", what = "character",
              quiet = TRUE, skip = feed.index)


    ## While there are still lines to read and less than n lines have been read.

    i = 1

    while(length(x) != 0 & i <= n){

        ## If there is an add change current position, add something into ID, and
        ## increment current position.

        if (x[1]=="A"){
            ob.data[current.pos,] = c(x[4], x[5], x[6], x[2], x[3])
            ids[x[3]] = current.pos
            current.pos = current.pos + 1
        }

        ## For a cancel remove the row from ob.data, remove the ID from list.

        if (x[1]=="C"){
            ob.data[ids[[x[3]]],][1] = NA
            ids[x[3]] = NULL
        }

        ## For a replace find the right row and replace it with the new size.

        if (x[1]=="R"){
            ob.data[ids[[x[3]]],][2] = x[4]
        }

        ## For a trade increment the trade index and store the trade data.

        if (x[1] == "T"){
            trade.data[as.character(feed.index)] = list(x)
        }

        ## Increase the feed index to keep track of which line we are on.

        feed.index = feed.index + 1
        i = i + 1
        x <- scan(feed, nline = 1, sep = ",", what = "character", quiet = TRUE)
    }

    ob@ob.data <- ob.data
    ob@feed.index <- feed.index
    ob@trade.data <- trade.data


    close(feed)

    ob = .update(ob, 0)

    invisible(ob)
}
