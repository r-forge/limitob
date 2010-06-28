\name{read.order.function}
\alias{read.orders,orderbook-method}
\alias{read.orders}

\title{read.order}
\description{Read the next n orders of the data file.}
\usage{
read.orders(object, n=1000)
}
\arguments{
  \item{object}{ Object of class \code{"orderbook"}}
  \item{n}{ Object of class \code{"numeric"}. The number of orders we
    want to read.}
}
\details{The orderbook keeps track of its current position within the
  data file. The read.orders function will read and
  process the next n orders from the file.
}
\value{
  Return an object of class \code{orderbook}
}
\examples{
## The examples must be run in the same directory as the sample.txt input file

## ob <- orderbook(file="sample.txt")
## ob <- read.orders(ob, n=100)
}
\keyword{file}