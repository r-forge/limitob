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

.plot.lattice.order.book <-function(object, bounds){

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

## Incomplete

.plot.side.order.book <-function(object){

    x = .combine.size(object)
    ob.names = object@ob.names

    im = inside.market(object)
    best.ask = im[[ob.names[1]]][1]
    best.bid = im[[ob.names[1]]][2]


    max.size = max(x[[ob.names[2]]])
    min.price.diff = signif(min(x[ob.names[[1]]])-.05,3)-best.bid
    max.price.diff = round(max(x[ob.names[[1]]])+0.5)-best.ask

    low.diff = .25

    panel.bestbid<- function(x = panel.args$x[panel.args$y == max(y)] + max.size/10,
                             y = panel.args$y,
                             panel.args = trellis.panelArgs())
    {
        panel.text(x = x, y = formatC(max(y), format = "f", digits = 2), labels = max(y), cex = 0.75, col = "red")
    }

    panel.bestask<- function(x = panel.args$x[panel.args$y == min(y)] + max.size/10,
                             y = panel.args$y,
                             panel.args = trellis.panelArgs())
    {
        panel.text(x = x, y = formatC(min(y), format = "f", digits = 2), labels = min(y), cex = 0.75, col = "blue")
    }

   # panel.spread<- function(x = max(orderbook.df$size),
   #                         y = panel.args$y,
   #                         panel.args = trellis.panelArgs())
   # {
   #     panel.text(x = min(x), y = min(y)-spread*1.5, labels = spread, cex = 0.75, col = "green")
   # }

    x.limits =  list(c(1000,0),
    c(0,1000))

    x.at = seq(0, 1000, 100)

    y.limits = list(c(best.bid - low.diff, best.bid),
    c(best.ask + low.diff, best.ask))

    yask.at = vector()
    ybid.at = vector()

    new.yscale.components <- function(...) {
        ans <- yscale.components.default(...)
        ans$right <- ans$left

        ans$right$labels$labels <- seq(best.bid - low.diff, best.bid, 0.01)
        ans$left$labels$labels <- seq(best.ask + low.diff, best.ask, -0.01)

        ans
    }

    x[[ob.names[3]]] = ordered(x[[ob.names[3]]], levels = c(ob.names[7], ob.names[6]))

    tmp <- xyplot(x[[ob.names[1]]]~x[[ob.names[2]]]|x[[ob.names[3]]], data = x,
                  ylab = "Price", xlab = "Size (Shares)", main = "Order Book",
                  scales = list(x = list(relation = "free",
                                limits = x.limits,
                                at = x.at,
                                axs = "i"),
                  y = list(relation = "free",
                  limits = y.limits, rot = 0, tick.number = 25)),

                  layout.widths = list(between = 5),
                  panel = function(...){
                      panel.xyplot(...)
                      panel.lines(..., type = "H")
                  }
                  )
    plot(tmp)

    trellis.focus("panel", 1, 1)
    panel.bestbid()
    Sys.sleep(0.01)
    trellis.unfocus()

    trellis.focus("panel", 2,1)
    panel.bestask()
    Sys.sleep(0.01)
    trellis.unfocus()
}


.plot.orders.order.book <-function(object){

    x = object@current.ob
    ob.names = object@ob.names
    x = x[x[[ob.names[1]]] < mid.point(object)*1.1 & x[[ob.names[1]]] > mid.point(object)*0.9,]

    type = .combine.size(object)
    type = table(type[[ob.names[3]]])

    x = data.frame(table(x[[ob.names[1]]]))
    x$Var1 = levels(x$Var1)
    x$Var1 = as.numeric(x$Var1)
    x = data.frame(x, "Type" = c(
                   rep("BID", type[[ob.names[7]]]),
                   rep("ASK", type[[ob.names[6]]])))

    names(x) = c(ob.names[1], "Orders", ob.names[3])




    max.orders = max(x[["Orders"]])
    min.price = signif(min(x[ob.names[[1]]])-.05,3)
    max.price = round(max(x[ob.names[[1]]])+0.5)

    panel.bestbid<- function(x = panel.args$x[panel.args$y == max(y)] + max.orders/10,
                             y = panel.args$y,
                             panel.args = trellis.panelArgs())
    {
        panel.text(x = x, y = formatC(max(y), format = "f", digits = 2), labels = max(y), cex = 0.75, col = "red")
    }

    panel.bestask<- function(x = panel.args$x[panel.args$y == min(y)] + max.orders/10,
                             y = panel.args$y,
                             panel.args = trellis.panelArgs())
    {
        panel.text(x = x, y = formatC(min(y), format = "f", digits = 2), labels = min(y), cex = 0.75, col = "blue")
    }

   # panel.spread<- function(x = max(orderbook.df$size),
   #                         y = panel.args$y,
   #                         panel.args = trellis.panelArgs())
   # {
   #     panel.text(x = min(x), y = min(y)-spread*1.5, labels = spread, cex = 0.75, col = "green")
   # }

    x.limits =  list(c(max.orders+max.orders/20,0),
    c(0,max.orders+max.orders/20))
    x.at = seq(0, max.orders, max.orders/5)
    tmp.at = seq(min.price, max.price, .1)
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

    x[[ob.names[3]]] = ordered(x[[ob.names[3]]], levels = c(ob.names[7], ob.names[6]))

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
    plot(tmp)

    trellis.focus("panel", 1, 1)
    panel.bestbid()

    Sys.sleep(0.01)
    trellis.unfocus()

    trellis.focus("panel", 2,1)
    panel.bestask()

    Sys.sleep(0.01)
    trellis.unfocus()
}

