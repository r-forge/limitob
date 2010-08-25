% A bad name for this function.

\name{initialize.trades}

\alias{initialize.trades,orderbook-method}

\alias{initialize.trades}

\title{initialize.trades}
\description{Calculate returns for trades.}
\usage{
initialize.trades(object, time = c(5, 300))
}
\arguments{
  \item{object}{Object of class \code{orderbook}}
  \item{time}{An integer. Can be a vector of integers.}
}
\details{
  This function calculates midpoint and trade weighted average price (TWAP)
  returns for the specified time in seconds. For example, using
  \code{c(5,300)} would calculate the 5 second and 300 second midpoint
  and TWAP returns for all trades in the \code{my.trades} slot.
}
\value{
  Returns an \code{orderbook} object.
}
\examples{
library(orderbook)
filename <- system.file("extdata", "sample.txt", package = "orderbook")
ob <- orderbook(file = filename)
ob <- initialize.trades(ob)
}
\keyword{methods}
