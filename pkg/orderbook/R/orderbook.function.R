################################################################################
##
##
## orderbook.function.R: Returns an object of class limitob
##
##
## limitob is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## limitob is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with limitob.  If not, see <http://www.gnu.org/licenses/>.
################################################################################

## Returns an orderbook object. For input it takes a data frame, and names for
## price, size, type, time, id, as well as what ASK and BID are denoted as.

orderbook <- function(x = data.frame(),
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

        x[price] = as.numeric(x[,price])
        x[size] = as.numeric(x[,size])
        x[type] = as.factor(x[,type])
        x[time] = as.numeric(x[,time])
        x[id] = as.character(x[,id])

        ## Get rid of junk data--rows with no time information, rows that are
        ## not of type ask or bid, and rows where price/size is 0.

        x = x[!is.na(x[,time]),]
        x = x[x[type] == ask | x[type] == bid,]
        x = x[x[price] != 0,]
        x = x[x[size] != 0,]

        ## Store the current order book as current.ob, and the user inputted
        ## names as ob.names. We use ob.names throughout the rest of the
        ## package and here is where we assign the names!

        ob.names = c(price, size, type, time, id, ask, bid)

        ## Rearrange the rows of the current.ob data frame.

        current.ob = data.frame(x[[price]], x[[size]], x[[type]],
        x[[time]], x[[id]])

        names(current.ob) = c(price, size, type, time, id)


        ## Put a time on the order book. We assume that it is the last timestamp
        ## in the order book.

        end = max(x[[time]]) + 1

        ## Return a new orderbook object.

        invisible(new("orderbook",
                      current.ob   = current.ob,
                      current.time = end,
                      ob.names     = ob.names,
                      file         = file
                      ))
    } else {

        ## x was empty so just create an ``empty'' orderbook object.
        ob.names = c(price, size, type, time, id, ask, bid)

        current.ob = data.frame(NA, NA, NA, NA, NA)

        names(current.ob) = ob.names[1:5]

        invisible(new("orderbook",
                      current.ob   = current.ob,
                      current.time = 0,
                      ob.names     = ob.names,
                      ob.data      = hash(),
                      trade.data   = hash(),
                      trade.index  = 0,
                      file         = file
                      ))
    }

}

