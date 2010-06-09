library(kcm.orderbook)

load("sample.RData")


load("remove.order.test.1.RData")
stopifnot(
	isTRUE(identical(remove.order.1@current.ob,
	                 remove.order(ob, id=929626)@current.ob))
)

load("replace.order.test.1.RData")
stopifnot(
	isTRUE(identical(replace.order.1@current.ob,
	                 replace.order(ob, id=473046, size=500)@current.ob))
)

