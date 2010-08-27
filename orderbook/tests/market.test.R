## Simple tests for various functions in market.R.

library(orderbook)

filename <- system.file("extdata", "sample.txt", package = "orderbook")

ob <- orderbook(file = filename)
ob <- read.orders(ob, 500)

## Best bid

stopifnot(all.equal(25.89, best(ob, side = "BID")[[1]]))

## Best ask

stopifnot(all.equal(25.90, best(ob, side = "ASK")[[1]]))

## Bid Price Levels

stopifnot(all.equal(31, o.and.l(ob, "bid", "levels")))

## Ask Price Levels

stopifnot(all.equal(29, o.and.l(ob, "ask", "levels")))

## Total Price Levels

stopifnot(all.equal(60, o.and.l(ob, "all", "levels")))

## Bid orders

stopifnot(all.equal(42, o.and.l(ob, "bid", "orders")))

## Ask orders

stopifnot(all.equal(40, o.and.l(ob, "ask", "orders")))

## Total orders

stopifnot(all.equal(82, o.and.l(ob, "all", "orders")))

## Midpoint

stopifnot(all.equal(25.895, mid.point(ob)))

## Inside.market. A hacky test case. Is this really the type of object
## that inside.market should return?

test <- inside.market(ob)

stopifnot(all.equal(25.90, test[[1]]))
stopifnot(all.equal(25.89, test[[2]]))
stopifnot(all.equal(600, test[[3]]))
stopifnot(all.equal(500, test[[4]]))

## Spread

stopifnot(all.equal(0.01, spread(ob)))

