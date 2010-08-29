library(orderbook)
load("agg.price.levels.test.RData")

filename <- system.file("extdata", "sample.txt", package = "orderbook")


ob <- orderbook(file = filename)
ob <- read.messages(ob, 1000)

## shares.0.001 <- agg.price.levels(ob, bounds = 0.001, kind = "shares")
## orders.0.001 <- agg.price.levels(ob, bounds = 0.001, kind = "orders")
## save(list = c("shares.0.001", "orders.0.001"), file = "agg.price.levels.test.RData")

stopifnot(all.equal(agg.price.levels(ob, bounds = 0.001, kind = "shares"),
                    shares.0.001))

stopifnot(all.equal(agg.price.levels(ob, bounds = 0.001, kind = "orders"),
                    orders.0.001))
