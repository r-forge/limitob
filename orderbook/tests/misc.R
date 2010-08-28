library(orderbook)
load("read.test.RData")

filename <- system.file("extdata", "sample.txt", package = "orderbook")

ob <- orderbook(file = filename)
ob <- read.messages(ob, 5000)

## Best bid

stopifnot(all.equal(25.79, best(ob, side = "BID")[[1]]))

## Best ask

stopifnot(all.equal(25.86, best(ob, side = "ASK")[[1]]))

## Bid Price Levels

stopifnot(all.equal(54, o.and.l(ob, "bid", "levels")))

## Ask Price Levels

stopifnot(all.equal(58, o.and.l(ob, "ask", "levels")))

## Total Price Levels

stopifnot(all.equal(112, o.and.l(ob, "all", "levels")))

## Bid orders

stopifnot(all.equal(104, o.and.l(ob, "bid", "orders")))

## Ask orders

stopifnot(all.equal(135, o.and.l(ob, "ask", "orders")))

## Total orders

stopifnot(all.equal(239, o.and.l(ob, "all", "orders")))

## Midpoint

stopifnot(all.equal(25.825, mid.point(ob)))

## Inside.market

test <- inside.market(ob)

stopifnot(all.equal(25.86, test[[1]]))
stopifnot(all.equal(25.79, test[[2]]))
stopifnot(all.equal(200, test[[3]]))
stopifnot(all.equal(600, test[[4]]))

## Spread

stopifnot(all.equal(0.07, spread(ob)))

## "["

ids = c("1170953", "4283097")

stopifnot(all.equal(ids, ob["25.79"]$id))

## Initialize Trades test

test <- c(-0.025, -0.025, -0.015, 0.005, 0.005, 0.005, -0.025, -0.005)

ob <- trade.returns(ob, c(5, 10))
midpoint.10 <- ob@my.trades$midpoint.10

## Calling this "test"?! Hack!

stopifnot(all.equal(midpoint.10, test))

## Reset test. Not a very ambition test.

ob <- reset(ob)

stopifnot(all.equal(0, ob@current.time))
stopifnot(all.equal(0, ob@file.index))
stopifnot(all.equal(0, nrow(ob@current.ob)))
