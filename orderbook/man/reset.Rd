\name{reset}
\alias{reset}
\title{
 Reset an object of class `orderbook'
}
\description{
  Removes the current.ob slot from an \code{orderbook} object and
  replaces it with an empty data frame.
}
\usage{
reset(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    An object of class `orderbook'.
  }
}
\value{
  An object of class `orderbook'.
}
\author{
  David Kane
}
\note{
Only used by read.time() and read.orders().
}


\seealso{
\code{\link{read.time}} and \code{\link{read.orders}}
}
\examples{
library(orderbook)
filename <- system.file("extdata", "sample.txt", package = "orderbook")
ob <- orderbook(file = filename)
ob <- read.orders(ob, 500)
x <- reset(ob)
}
\keyword{misc}
