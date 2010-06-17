library(kcm.orderbook)
load("inside.market.test.RData")
load("sample.RData)
stopifnot(
	isTRUE(identical(inside.market(ob, invis = TRUE), inside.market.test.df))
) 
