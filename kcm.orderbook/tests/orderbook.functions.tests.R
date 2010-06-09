#Khanh Nguyen
library(kcm.orderbook)

load("sample.RData")
stopifnot(
	isTRUE(all.equal(bid.price.levels(ob), 24))
)

stopifnot(
	isTRUE(all.equal(ask.price.levels(ob), 15))
)

stopifnot(
	isTRUE(all.equal(total.price.levels(ob), 39))
)

stopifnot(
	isTRUE(all.equal(bid.orders(ob), 28))
)

stopifnot(
	isTRUE(all.equal(ask.orders(ob), 20))
)

stopifnot(
	isTRUE(all.equal(total.orders(ob), 48))
)
stopifnot(
	isTRUE(all.equal(mid.point(ob), c(price = 27.285)))
)
stopifnot(
	isTRUE(all.equal(spread(ob), 0.05))
)