library(orderbook)

filename <- system.file("data", "sample.txt",
                        package = "orderbook")

ob <- orderbook(file = filename, trader = FALSE)
ob <- read.orders(ob, 5000)
ob <- next.trade(ob)

## Next trade

stopifnot(isTRUE(identical(5044, ob@file.index)))

## Trade after

ob <- next.trade(ob)

stopifnot(isTRUE(identical(5046, ob@file.index)))

## Trade before

ob <- previous.trade(ob)

stopifnot(isTRUE(identical(5044, ob@file.index)))

## Trade before

ob <- previous.trade(ob)

stopifnot(isTRUE(identical(4993, ob@file.index)))
