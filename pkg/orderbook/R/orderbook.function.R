################################################################################
##
##
## orderbook.function.R: Returns an object of class limitob
##
##
## orderbook is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## orderbook is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with orderbook.  If not, see <http://www.gnu.org/licenses/>.
################################################################################

## Returns an orderbook object. For input it takes a data frame, and names for
## price, size, type, time, id, as well as what ASK and BID are denoted as.

orderbook <- function(x     = data.frame(),
                      price = "price",
                      size  = "size",
                      type  = "type",
                      time  = "time",
                      id    = "id",
                      ask   = "ASK",
                      bid   = "BID",
                      file  = NULL)
{

    ## Make sure the user inputted correct names for the columns, ie the
    ## columns named actually exist in data frame x. If x isn't empty do the
    ## following.

    if(nrow(x) != 0){
        if(!(id %in% names(x) &
             price %in% names(x) &
             size %in% names(x) &
             type %in% names(x) &
             time %in% names(x))){
            stop("Wrong variable name")
        }



        ## Casting everything to make sure it is the desired data type. For some
        ## reason ID doesn't really get casted so I'll cast it again later.

        x[price] <- as.numeric(x[,price])
        x[size] <- as.numeric(x[,size])
        x[type] <- as.factor(x[,type])
        x[time] <- as.numeric(x[,time])
        x[id] <- as.character(x[,id])

        ## Get rid of junk data--rows with no time information, rows that are
        ## not of type ask or bid, and rows where price/size is 0.

        x <- x[!is.na(x[,time]),]
        x <- x[x[type] == ask | x[type] == bid,]
        x <- x[x[price] != 0,]
        x <- x[x[size] != 0,]

        ## Rearrange the rows of the current.ob data frame and rename the columns.

        current.ob <- data.frame(x[[price]], x[[size]], x[[type]],
                                 x[[time]], x[[id]])

        names(current.ob) <- c("price", "size", "type", "time", "id")

        ## Rename to "ASK" and "BID"

        x[type][x[type] == ask] = "ASK"
        x[type][x[type] == bid] = "BID"


        ## Put a time on the order book. We assume that it is the last timestamp
        ## in the order book.

        end <- max(x[[time]]) + 1

        ## Return a new orderbook object.

        invisible(new("orderbook",
                      current.ob   = current.ob,
                      current.time = end,
                      ob.data      = hash(),
                      trade.data   = hash(),
                      file         = file,
                      my.trades    = hash()
                      ))
    } else {

        ## x was empty so just create an ``empty'' orderbook object.

        current.ob <- data.frame(price = numeric(0), size =
                                 numeric(0), type = character(0), time
                                 = numeric(0), id = character(0))

        invisible(new("orderbook",
                      current.ob   = current.ob,
                      current.time = 0,
                      ob.data      = hash(),
                      trade.data   = hash(),
                      file         = file,
                      my.trades    = hash()
                      ))
    }

}

