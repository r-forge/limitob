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
\details{Detail here}
\value{
  Return an object of class \code{orderbook}
}
\keyword{file}