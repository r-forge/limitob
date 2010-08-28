\name{market}
\alias{inside.market}
\alias{o.and.l}
\alias{best}
\alias{spread}
\alias{mid.point}
\title{
Functions relating to aggregate book data
}
\description{
Collection of functions for looking at data that has been aggregated up
from the underlying orders.
}
\usage{
inside.market(x)
o.and.l(x, side, type)
best(x, side)
spread(x)
mid.point(x)
}
\arguments{
  \item{x}{
Object of class \code{orderbook}.
}
  \item{side}{
Side of interest. Must be one of "bid", "ask" or "all".
}
  \item{type}{
Orders or levels.
}
}
\value{
Either a single number or a larger object (vector or data frame)  of
more information.
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
ob <- read.messages(ob, n = 1000)
inside.market(ob)
o.and.l(ob, side = "BID", type = "orders")
best(ob, side = "ASK")
spread(ob)
mid.point(ob)
}
\keyword{misc}

