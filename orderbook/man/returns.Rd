\name{trade.returns}
\alias{trade.returns}
\alias{midpoint.return}
\alias{twap.return}

\title{
  Returns associated with specific trades.
}
\description{
  Collection of functions for calculating returns of specific trades.
}
\usage{
trade.returns(x, time = c(5, 300))
}

\arguments{
  \item{x}{
    An object of class `orderbook'.
  }
  \item{time}{
    Number of seconds to calculate mid.point return for.
  }
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
\author{
  David Kane
}
\seealso{
  \code{\link{orderbook}}
}
\examples{
library(orderbook)
filename <- system.file("extdata", "sample.txt", package = "orderbook")
ob <- orderbook(file = filename)
ob <- trade.returns(ob)
}
\keyword{misc}

