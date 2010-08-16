\name{plot.function}

\alias{plot,orderbook-method}

\alias{plot}

\title{plot}
\description{Plotting the orderbook.}
\usage{
\S4method{plot}{orderbook}(x, bounds = 0.1, n = 10, type = "n")
}
\arguments{
  \item{x}{ Object of class \code{orderbook}}
  \item{bounds}{ Percentage above and below the midpoint to determine the y limits.}
  \item{n}{ Number of price levels to plot.}
  \item{type}{ Either \code{"n"}, \code{"s"}, \code{"t"}, or \code{"c"}.}
}
\details{This function plots the orderbook. \code{n} is only needed
  for \code{type = "s"}, which plots the best bid vs. the best ask, and
  then the second best bid vs. the second best ask, for \code{n} bids
  and asks. If \code{type = "n"} then the orderbook will be plotted with
  price levels on the y-axis, and size on the x-axis. \code{type = "o"}
  is similar, except the number of orders for each price level are
  plotted. \code{type = "t"} makes a plot that shows the size of the
  individual orders as well as the users own orders in the correct time priority.)

  plotTrade plots the number of shares traded at each price level.}
\value{
  Prints a Trellis object.
}
\examples{
library(orderbook)
filename <- system.file("data", "sample.txt",
                        package = "orderbook")

ob <- orderbook(file = filename)
ob <- read.orders(ob, n = 1000)
plot(ob)
plot(ob, type = "n")
}
\keyword{aplot}