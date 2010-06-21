\name{read.order.function}
\alias{read.orders,orderbook-method}
\alias{read.orders}

\title{read.order}
\description{Read the next n orders from the input feed from the feed's current position.}
\usage{
read.orders(object, n=1000)
}
\arguments{
  \item{object}{ Object of class \code{"orderbook"}}
  \item{n}{ Object of class \code{"numeric"}. For read.orders(), it is
    the number of orders we want to read from the current position in
    the feed.}
}
\details{The orderbook object internally keeps track of the current position in the orderbook feed. 
The read.orders function will read and process the next n rows from the feed. By defaul, the number
of orders to be read is the next 1000 orders from the current position in the feed. If there are less than 1000
orders in the feed from the current position, the function reads until the end of the feed file.
}
\value{
  Return an object of class \code{orderbook}
}
\examples{
# the codes must be run in the same directory with the sample.txt input feed

#ob <- orderbook(feed="sample.txt")
#ob <- read.orders(ob, n=100)
}
\keyword{file}