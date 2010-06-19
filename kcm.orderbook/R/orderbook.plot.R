################################################################################
##
##
## orderbook.plot.R: Helper function for the plot method
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

## Plots the orderbook object at current time. Displays Bids on the left and
## Asks on the right with Price and Size on the Y- and X-axes, respectively.
## Only prices within 10% above and below the midpoint value are shown.

.plot.ob <-function(object, bounds){

    ## Use combine size to find the total size at each price level. This
    ## function returns a data frame. Also get the names for the columns.

    x = .combine.size(object, bounds)
    ob.names = object@ob.names

    ## If there is nothing on the orderbook, stop
    stopifnot(nrow(x)>0)




    ## Maximum size, max/min price. and difference between the max
    ## and min price for purposes of drawing the axes.

    max.size = signif(max(x[[ob.names[2]]]), 2)
    min.price = signif(min(x[ob.names[[1]]])-.05,3)
    max.price = round(max(x[ob.names[[1]]])+0.5)
    midpoint = mid.point(object)

    ## Panel functions that display the best bid and best ask.

    panel.bestbid<- function(x = max.size/2,
                             y = panel.args$y,
                             panel.args = trellis.panelArgs())
    {
        panel.text(x = x, y = max(y) + midpoint * bounds/20,
                   labels = formatC(max(y), format = "f", digits = 2),
                   cex = 0.75, col = "red")
    }

    panel.bestask<- function(x = max.size/2,
                             y = panel.args$y,
                             panel.args = trellis.panelArgs())
    {
        panel.text(x = x, y = min(y) - midpoint * bounds/20,
                   labels = formatC(min(y), format = "f", digits = 2),
                   cex = 0.75, col = "blue")
    }

    ## Creating the x axis values.

    x.limits =  list(c(max.size+max.size/20,0),
    c(0,max.size+max.size/20))
    x.at = seq(0, max.size, max.size/5)

    ## Creating the y axis values.

    tmp.at = formatC(seq(min.price, max.price, .1), format = "f", digits = 2)
    yask.at = vector()
    ybid.at = vector()

    for(i in 1:length(tmp.at)){
  	if(i%%2==0){
            yask.at[i] = tmp.at[i]
            ybid.at[i]=""
  	}else{
            yask.at[i] = ""
	   	ybid.at[i] = tmp.at[i]
   	}
    }

    new.yscale.components <- function(...) {
        ans <- yscale.components.default(...)
        ans$right <- ans$left
        ans$left$labels$labels <- ybid.at
        ans$right$labels$labels <- yask.at
        ans
    }

    ## Ordering the levels so Bid comes before Ask, this allows Bid to be
    ## on the left.

    x[[ob.names[3]]] = ordered(x[[ob.names[3]]], levels = c(ob.names[7],
                                                 ob.names[6]))

    ## Actually plotting it.

    tmp <- xyplot(x[[ob.names[1]]]~x[[ob.names[2]]]|x[[ob.names[3]]], data = x,
                  ylab = "Price", xlab = "Size (Shares)", main = "Order Book",
                  scales = list(x = list(relation = "free",
                                limits = x.limits,
                                at = x.at,
                                axs = "i"),
                  y = list(at = tmp.at, alternating = 3)),
                  yscale.components = new.yscale.components,
                  panel = function(...){
                      panel.xyplot(...)
                      panel.lines(..., type = "H")
                  }
                  )

    ## Printing the plot.

    print(tmp)

    ## Using the panel functions to add the best ask and best bid.

    trellis.focus("panel", 1, 1)
    panel.bestbid()
    trellis.unfocus()

    trellis.focus("panel", 2,1)
    panel.bestask()
    trellis.unfocus()
}

## Plot top ask vs top bid, 2nd best ask vs 2nd best bid, etc.

.plot.side.ob <-function(object, n){

    x = .combine.size(object, 1)
    ob.names = object@ob.names

    ## Creating the data frame to be plotted.

    ask = x[x[[ob.names[3]]] == ob.names[6],][1:n,]

    bid = x[x[[ob.names[3]]] == ob.names[7],]
    bid = bid[(nrow(bid) - n + 1):nrow(bid),]

    x = rbind(ask, bid)
    x$y = c(seq(n, 1, -1), seq(1, n, 1))

    ## Setting x-axis limits and labels.
    max.size = signif(max(x[[ob.names[2]]]), 1)
    x.limits = c(0, signif(max.size, 1))
    x.at = seq(0, signif(max.size), max.size/5)

    ## Creating y-axis tick labels.

    ask.label <- formatC(sort(ask[[ob.names[1]]], decreasing = TRUE),
                         format = "f", digits = 2)

    bid.label <- formatC(bid[[ob.names[1]]], format = "f", digits = 2)

    new.yscale.components <- function(...) {
        ans <- yscale.components.default(...)
        ans$right <- ans$left

        ans$right$labels$labels <- ask.label
        ans$left$labels$labels <- bid.label

        ans
    }

    new.par.settings = list(
    layout.widths = list(left.padding = 2, right.padding = 5))

    tmp <- barchart(y ~ size, data = x, groups = type, auto.key = TRUE,
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

    trellis.focus("panel", 1, 1, clip.off = TRUE)
    grid.text("Ask Price Levels", x = 1.12, rot = 90)
    trellis.unfocus()

}


.plot.orders.ob <-function(object, bounds){

    x <- object@current.ob
    ob.names <- object@ob.names

    ## We only want data within our bounds

    x <- x[(x[[ob.names[1]]] < mid.point(object)*(1+bounds)
           & x[[ob.names[1]]] > mid.point(object)*(1-bounds)),]

    ## Create data.frame with price level, number of orders, and type

    ask <- x[x[[ob.names[3]]] == ob.names[6],]
    bid <- x[x[[ob.names[3]]] == ob.names[7],]

    ask <- data.frame(table(ask[[ob.names[1]]]))
    bid <- data.frame(table(bid[[ob.names[1]]]))

    ask = cbind(ask, rep("ASK", nrow(ask)))
    bid = cbind(bid, rep("BID", nrow(bid)))

    names(ask) = c(ob.names[1], "Orders", ob.names[3])
    names(bid) = names(ask)

    x = rbind(ask, bid)
    x[[ob.names[1]]] = as.numeric(levels(x[[ob.names[1]]]))

    ## Maximum size, max/min price. and difference between the max
    ## and min price for purposes of drawing the axes.

    max.orders = signif(max(x[["Orders"]]), 1)
    min.price = signif(min(x[ob.names[[1]]])-.05, 3)
    max.price = round(max(x[ob.names[[1]]])+0.5)
    midpoint = mid.point(object)

    ## Panel functions that display the best bid and best ask.

    panel.bestbid <- function(x = max.orders/2,
                             y = panel.args$y,
                             panel.args = trellis.panelArgs())
    {
        panel.text(x = x, y = max(y) + midpoint * bounds/20,
                   labels = formatC(max(y), format = "f", digits = 2),
                   cex = 0.75, col = "red")
    }

    panel.bestask <- function(x = max.orders/2,
                             y = panel.args$y,
                             panel.args = trellis.panelArgs())
    {
        panel.text(x = x, y = min(y) - midpoint * bounds/20,
                   labels = formatC(min(y), format = "f", digits = 2),
                   cex = 0.75, col = "blue")
    }

    ## Create x axes/limits.

    x.limits =  list(c(max.orders ,0),
    c(0, max.orders))
    x.at = seq(0, max.orders, 10)

    ## Create y axes/limits.

    tmp.at = formatC(seq(min.price, max.price, .1), format = "f", digits = 2)
    yask.at = vector()
    ybid.at = vector()

    for(i in 1:length(tmp.at)){
  	if(i%%2==0){
            yask.at[i] = tmp.at[i]
            ybid.at[i]=""
  	}else{
            yask.at[i] = ""
	   	ybid.at[i] = tmp.at[i]
   	}
    }

    new.yscale.components <- function(...) {
        ans <- yscale.components.default(...)
        ans$right <- ans$left
        ans$left$labels$labels <- ybid.at
        ans$right$labels$labels <- yask.at
        ans
    }

    ## Ordering the levels so Bid comes before Ask.

    x[[ob.names[3]]] <- ordered(x[[ob.names[3]]],
                                levels = c(ob.names[7], ob.names[6]))

    ## Actually plotting it.

    tmp <- xyplot(x[[ob.names[1]]]~x[["Orders"]]|x[[ob.names[3]]], data = x,
                  ylab = "Price", xlab = "Number of Orders", main = "Order Book",
                  scales = list(x = list(relation = "free",
                                limits = x.limits,
                                at = x.at,
                                axs = "i"),
                  y = list(at = tmp.at, alternating = 3)),
                  yscale.components = new.yscale.components,
                  panel = function(...){
                      panel.xyplot(...)
                      panel.lines(..., type = "H")
                  }
                  )

    ## Printing the plot

    print(tmp)

    ## Using the panel functions to add the best ask and best bid.

    trellis.focus("panel", 1, 1)
    panel.bestbid()
    trellis.unfocus()

    trellis.focus("panel", 2,1)
    panel.bestask()
    trellis.unfocus()
}

