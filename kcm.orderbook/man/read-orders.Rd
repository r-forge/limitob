\name{read-order-functions}
\alias{read.orders}
\alias{read.time}
\alias{reset}
\alias{view.trade}

\alias{read.orders, orderbook-method}
\alias{read.time, orderbook-method}
\alias{reset, orderbook-method}
\alias{view.trade, orderbook-method}

\title{Moving inside the orderbook}
\description{functions to walk around orders in the orderbook}
\usage{
read.orders(object, n=1000, ...)
read.time(object, n, ...)
reset(object, ...)
view.trade(object, n=1, ...)
}
\arguments{
  \item{object}{ Object of class \code{"orderbook"}}
  \item{n}{ Object of class \code{"numeric"}. For read.time(), it is the
    number of milliseconds after midnight UTC. For read.orders(), it is
    the number of orders we want to read from the current position in
    the feed. For view.trade(), it is the trade index. }    
}
\details{Detail here}
\value{
  Return an object of class \code{orderbook}
}
\keyword{file}