\name{sort.function}

\alias{sort,orderbook-method}
\alias{sort}

\title{sort}
\description{Sorting orderbook trades.}

\usage{
\S4method{sort}{orderbook}(x, by, decreasing = TRUE)
}

\arguments{
    \item{x}{Object of class \code{orderbook}}
    \item{by}{The name of a column in \code{my.trades}.}
    \item{decreasing}{A \code{boolean}.}
  }

  \details{This method sorts \code{my.trades} by one of the
  columns. This is useful after running \code{trade.returns} for
  sorting by midpoint or trade weighted average price returns.
}
\value{
  Returns an \code{orderbook} object.
}
\examples{
library(orderbook)
filename <- system.file("extdata", "sample.txt", package = "orderbook")
ob <- orderbook(file = filename)
ob <- read.messages(ob, n = 1000)
ob <- sort(ob, by = "time", decreasing = TRUE)
}
\keyword{methods}
