% Good that an important function has its own .Rd file.

% n and t are bad argument names

\name{read.order.function}
\alias{read.orders,orderbook-method}
\alias{read.time,orderbook-method}

\alias{read.orders}
\alias{read.time}

\title{read.order}
\description{Read messages into the object.}
\usage{
read.orders(object, n = 1000)
read.time(object, t)
}
\arguments{
  \item{object}{ Object of class \code{orderbook}}
  \item{n}{An integer.}
  \item{t}{A time in HH:MM:SS format.}
}
\details{The orderbook keeps track of its current position within the
  data file. The \code{read.orders} function will read and
  process the next n messages from the input file. The \code{read.time}
  function will process all messsages in the input file before the
  specified time.
}
\value{
  Return an object of class \code{orderbook}
}
\examples{
library(orderbook)
filename <- system.file("extdata", "sample.txt", package = "orderbook")
ob <- orderbook(file = filename)
ob <- read.orders(ob, 100)
ob <- read.time(ob, "9:30:00")
}
\keyword{file}
