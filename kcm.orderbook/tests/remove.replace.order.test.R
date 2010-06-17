library(kcm.orderbook)

load("sample.RData")


load("remove.order.test.1.RData")
stopifnot(
	isTRUE(identical(remove.order.1@current.ob,
	                 remove.order(ob, id=52109)@current.ob))
)

load("replace.order.test.1.RData")
stopifnot(
	isTRUE(identical(replace.order.1@current.ob,
	                 replace.order(ob, id=52109, size=500)@current.ob))
)

