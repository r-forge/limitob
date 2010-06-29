## read.orders.test.R: Tests for read order functions
##
## limitob is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## limitob is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Rcpp.  If not, see <http://www.gnu.org/licenses/>.


library(orderbook)


#something is wrong. This returns TRUE in R, but fail with test
ob <- orderbook(file = "sample.txt")

# read 20 orders
ob <- read.orders(ob, n=20)

load("ob.next.20.RData")
stopifnot(
	isTRUE(identical(ob.next.20@current.ob,
	                 ob@current.ob))
)

# go back 5 orders
ob <- read.orders(ob, n=-5)
load("ob.go.back.5.RData")
stopifnot(
	isTRUE(identical(ob.go.back.5@current.ob,
	                 ob@current.ob))
)

# reset
ob <- reset(ob)
ob <- read.orders(ob, n=2000)
ob <- view.trade(ob, n=1)
load("ob.1st.trade.RData")
stopifnot(
	isTRUE(identical(ob.1st.trade@current.ob,
	                 ob@current.ob))
)
