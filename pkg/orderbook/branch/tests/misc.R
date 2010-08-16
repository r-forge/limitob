library(orderbook)
load("read.orders.time.test.RData")

filename <- system.file("data", "sample.txt",
                        package = "orderbook")

ob <- orderbook(file = filename)
ob <- read.orders(ob, 5000)

## Best bid

stopifnot(isTRUE(identical(25.79, best.bid(ob)[[1]])))

## Best ask

stopifnot(isTRUE(identical(25.86, best.ask(ob)[[1]])))

## Bid Price Levels

stopifnot(isTRUE(identical(as.integer(54), bid.price.levels(ob))))

## Ask Price Levels

stopifnot(isTRUE(identical(as.integer(58), ask.price.levels(ob))))

## Total Price Levels

stopifnot(isTRUE(identical(as.integer(54+58), total.price.levels(ob))))

## Bid orders

stopifnot(isTRUE(identical(104, bid.orders(ob))))

## Ask orders

stopifnot(isTRUE(identical(135, ask.orders(ob))))

## Total orders

stopifnot(isTRUE(identical(104+135, total.orders(ob))))

## Midpoint

stopifnot(isTRUE(identical(25.825, round(mid.point(ob)[[1]], 3))))

## Inside.market

test <- inside.market(ob, invis = TRUE)

stopifnot(isTRUE(identical(25.86, test[[1]])))
stopifnot(isTRUE(identical(25.79, test[[2]])))
stopifnot(isTRUE(identical(200, test[[3]])))
stopifnot(isTRUE(identical(600, test[[4]])))

## Spread

stopifnot(isTRUE(identical(0.07, round(spread(ob), 2))))

## "["

ids = c("1170953", "4283097")

stopifnot(isTRUE(identical(ids, ob["25.79"]$id)))

## Initialize Trades test

test <- c(-0.025, -0.025, -0.015, 0.005, 0.005, 0.005, -0.025, -0.005)

ob <- initialize.trades(ob, c(5, 10))
midpoint.10 <- ob@my.trades$midpoint.10

stopifnot(isTRUE(identical(midpoint.10, test)))

## Reset test

ob <- reset(ob)

stopifnot(isTRUE(identical(0, ob@current.time)))
stopifnot(isTRUE(identical(0, ob@file.index)))
stopifnot(isTRUE(identical(as.integer(0), nrow(ob@current.ob))))
