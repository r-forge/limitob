library(orderbook)


## remove order test
load("sample.RData")
stopifnot(
		isTRUE(52109 %in% ob@current.ob[[5]])
)
ob <- remove.order(ob, id=52109)
stopifnot(
		isTRUE(!(52109 %in% ob@current.ob[[5]]))
)


## replace order test
rm(ob)
load("sample.RData")
stopifnot(
		isTRUE(52109 %in% ob@current.ob[[5]])
)
ob <-  replace.order(ob, id=52109, size=500)
stopifnot(
		isTRUE(all.equal(get.order.info(ob, 52109)[2],c(size=500)))
)


