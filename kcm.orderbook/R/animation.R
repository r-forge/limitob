## "from" and "to" are strings in the form
## "HH::MM::SS", simiar to "by"
.animate <- function(ob, from, to, by="00:00:01"){
	
	## generate required time sequences
	## require chron package
	time.seq <- as.character(seq(times(from), times(to), by=by))
	
	## get all the trellis objects
	trellis.vector <- c()
	for (i in 1:length(time.seq)){
		trellis.vector[i] <- .animate.aux(read.time(ob, time.seq[i]))
	}	

	## animate....
}


.animate.aux <-function(object, bounds=0.1){

    ## Use combine size to find the total size at each price level. This
    ## function returns a data frame. Also get the names for the columns.

    x = .combine.size(object, bounds)
    ob.names = object@ob.names

    ## If there is nothing on the orderbook, stop
    stopifnot(nrow(x)>0)




    ## Maximum size, max/min price and difference between the max
    ## and min price for purposes of drawing the axes.

    max.size = max(x[[ob.names[2]]])
    max.size = ceiling(max.size + max.size/20)

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

    x.limits =  list(c(max.size,0), c(0,max.size))
    x.at = ceiling(seq(0, max.size, max.size/5))

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
	tmp
}
