library(kcm.orderbook)

load("sample.RData")

load("add.order.test1.RData") 
x = add.bid.1@current.ob
y = add.order(ob, type = "BID", price = 11, size = 1590)
y = y@current.ob

stopifnot(isTRUE(identical(x, y)))


load("add.order.test2.RData")
stopifnot(
	isTRUE(identical(add.ask.1@current.ob,
	                 add.order(ob, type="ASK", 
			                price=12.10, size=20000)@current.ob))
)
