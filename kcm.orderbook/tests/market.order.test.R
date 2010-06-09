library(kcm.orderbook)
load("sample.RData")


load("market.order.test.1.RData")
stopifnot(
	isTRUE(identical(market.order.test.1@current.ob,
	                 market.order(ob, type="BUY", size=100)@current.ob))
)

load("market.order.test.2.RData")
stopifnot(
	isTRUE(identical(market.order.test.2@current.ob,
	                 market.order(ob, type="BUY", size=500)@current.ob))
)

load("market.order.test.3.RData")
stopifnot(
	isTRUE(identical(market.order.test.3@current.ob,
	                 market.order(ob, type="SELL", size=1000)@current.ob))
)

load("market.order.test.4.RData")
stopifnot(
	isTRUE(identical(market.order.test.4@current.ob,
	                 market.order(ob, type="SELL", size=1150)@current.ob))
)