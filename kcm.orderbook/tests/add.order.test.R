library(kcm.orderbook)

load("sample.RData")

load("add.order.test.1.RData") 
x = add.bid.1@current.ob
y = add.order(ob, type = "BID", price = 26.80, size = 150)
y = y@current.ob

stopifnot(isTRUE(identical(x, y)))


load("add.order.test.2.RData")
x = add.bid.2@current.ob
y = add.order(ob, type="BID", price=27.22, size=15000)
y = y@current.ob

stopifnot(isTRUE(identical(x,y)))


load("add.order.test.3.RData")
stopifnot(
	isTRUE(identical(add.ask.1@current.ob,
	                 add.order(ob, type="ASK", 
			                price=27.30, size=123456)@current.ob))
)

load("add.order.test.4.RData")
stopifnot(
	isTRUE(identical(add.ask.2@current.ob,
	                 add.order(ob, type="ASK", 
			                price=27.60, size=123456)@current.ob))
)