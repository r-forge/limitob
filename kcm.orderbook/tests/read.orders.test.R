library(kcm.orderbook)


#something is wrong. This returns TRUE in R, but fail with test
ob <- orderbook(feed="sample.txt")

# read 20 orders
ob <- read.orders(ob, n=20)

load("ob.next.20.RData")
stopifnot(
	isTRUE(identical(ob.next.20@current.ob,
	                 ob@current.ob))
)

# go back 5 orders
ob <- read.orders(ob, n=-5)
load("ob.go.back.5.RData")
stopifnot(
	isTRUE(identical(ob.go.back.5@current.ob,
	                 ob@current.ob))
)

# reset 

ob <- reset(ob)
ob <- read.orders(ob)
ob <- view.trade(ob, n=1)
load("ob.1st.trade.RData")
stopifnot(
	isTRUE(identical(ob.1st.trade@current.ob,
	                 ob@current.ob))
)