\name{plot.function}

\alias{plot,orderbook-method}
\alias{plotTrade,orderbook-method}

\alias{plot}
\alias{plotTrade}

\title{plot}
\description{Plotting the orderbook.}
\usage{
\S4method{plot}{orderbook}(x, bounds = 0.1, n = 10, type = "n")
plotTrade(x)
}
\arguments{
  \item{x}{ Object of class \code{"orderbook"}}
  \item{bounds}{ Object of class \code{"numeric"}. Percentage above and
    below the midpoint to determine the y limits.}
  \item{n}{ Object of class \code{"numeric"}. Number of price levels to
    plot.}
  \item{type}{ Object of class \code{"character"}. Either \code{"n"},
    \code{"s"}, or \code{"c"}.}
}
\details{This function plots the orderbook. \code{"n"} is only needed
  for \code{"type = s"}, which plots the best bid vs. the best ask, and
  then the second best bid vs. the second best ask, for \code{"n"} bids
  and asks. If \code{"type = n"} then the orderbook will be plotted with
  price levels on the y-axis, and size on the x-axis. \code{"type = o"}
  is similar, except the number of orders for each price level are
  plotted.

  plotTrade plots the number of shares traded at each price level.}
\value{
  Prints a Trellis object.
}
\examples{
## The examples must be run in the same directory as the sample.txt input file

## ob <- orderbook(file = "sample.txt")
## ob <- read.orders(ob, n = 1000)
## plotTrade(ob)
## plot(ob)
}
\keyword{aplot}