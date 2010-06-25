\name{read.time.function}

\alias{read.time,orderbook-method}
\alias{read.time}

\title{read.time}
\description{Returns the state of the orderbook at the specified time.}

\usage{
object = read.time(object, n)
}
\arguments{
  \item{object}{Object of class \code{"orderbook"}}
  \item{n}{Object of class \code{"character"}. A string in the form \code{"HH:MM:SS"}.}
}
\details{Displays the orderbook at the specified time. Can be used to
  move backwards or forwards in time.}
\value{
  Return an object of class \code{orderbook}
}
\examples{
## The examples must be run in the same directory as the sample.txt input feed

## ob <- orderbook(feed="sample.txt")
## ob <- read.orders(ob, n=1000)
## ob <- read.time(ob, "09:30:00")
}
\keyword{file}