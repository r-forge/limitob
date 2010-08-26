o.and.l <- function(x, side, type){

    ## Query an orderbook object and find out key data from it.

    side <- toupper(side)
    type <- tolower(type)

    stopifnot(class(x) == "orderbook")
    stopifnot(side %in% c("BID", "ASK", "ALL"))
    stopifnot(type %in% c("levels", "orders"))

    ## Grab out the key dataframe.

    x <- x@current.ob

    ## Pull out the side unless ALL.

    if(side != "ALL"){
        x <- x[x[["type"]] == side,]
    }

    ## Measure levels or total orders, as required.

    if(type == "orders"){
        y <- max(0, nrow(x))
    }
    else{
        ## Use table to count. Better way?

        y <- length(table(x[["price"]], exclude = NA))
    }


    y
}
