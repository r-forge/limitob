\name{agg.price.levels}
\alias{agg.price.levels}
\title{
Aggregate up shares/orders for each price level.
}
\description{
For each price level in an orderbook object that is within a specified
percentage of the midpoint, aggegate up the number of shares or orders
at each price level. Mainly used in plotting functions.
}
\usage{
agg.price.levels(x, bounds, kind)
}
\arguments{
  \item{x}{
    An object of class \code{orderbook}.
  }
  \item{bounds}{
    Range around midpoint to include. 0.1 means 10\% above and below midpoint.
  }
  \item{kind}{
    Either `shares' or `orders'.
  }
}
\value{
  A data frame with three columns: price, shares (or orders), and type
  (which is either `BID' or `ASK').
}
\author{
  David Kane
}

\examples{
library(orderbook)
filename <- system.file("extdata", "sample.txt", package = "orderbook")
ob <- orderbook(file = filename)
ob <- read.messages(ob, 2000)
x <- agg.price.levels(ob, bounds = 0.001, kind = "shares")
x <- agg.price.levels(ob, bounds = 2, kind = "orders")
}
\keyword{misc}

