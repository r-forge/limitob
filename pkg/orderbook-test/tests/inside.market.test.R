library(orderbook)

load("sample.RData")

tt <- inside.market(ob)

stopifnot(
	isTRUE(all.equal(tt[1,1], 11.53))
)

stopifnot(
	isTRUE(all.equal(tt[1,2], 100))
)

stopifnot(
	isTRUE(all.equal(tt[2,1], 11.35))
)

stopifnot(
	isTRUE(all.equal(tt[2,2], 1000))
)
