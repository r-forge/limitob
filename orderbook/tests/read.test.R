## read.test.R: Tests for read.messages and read.time functions.

## Comments: (Feel free to delete these as the tests are redone.)
## First, we need some simple test cases that we have checked by hand
## to ensure that they work, something with an input file that just
## contains a dozen or so rows. Second, we need to check each output
## object after each command is given. Running read.messages(ob, -5)
## and then read.messages(ob, 5) WITHOUT CHECKING THE INTERMEDIATE
## OBJECT does not really test anything.

library(orderbook)
load("read.test.RData")

filename <- system.file("extdata", "sample.txt", package = "orderbook")

ob <- orderbook(file = filename)

# read 5000 orders
ob <- read.messages(ob, 5000)

stopifnot(isTRUE(identical(test, ob@current.ob)))


# go back 5 orders and then forward 5 orders
ob <- read.messages(ob, -5)
ob <- read.messages(ob, 5)

stopifnot(isTRUE(identical(test, ob@current.ob)))

## Read time, should take us to 4981 orders

ob <- read.time(ob, "9:29:59")
ob <- read.messages(ob, 361)

stopifnot(isTRUE(identical(test, ob@current.ob)))
