library(orderbook)


## add bid order test
load("sample.RData")
stopifnot(
		isTRUE(!(123456 %in% ob@current.ob[[5]]))
)
ob <- add.order(ob, type="BID", price=11, size=1590, id=123456)
stopifnot(
		isTRUE(123456 %in% ob@current.ob[[5]])
)

## add ask order test
rm(ob)
load("sample.RData")
stopifnot(
		isTRUE(!(123456 %in% ob@current.ob[[5]]))
)
ob <- add.order(ob, type="ASK", price=12, size=1590, id=123456)
stopifnot(
		isTRUE(123456 %in% ob@current.ob[[5]])
)

