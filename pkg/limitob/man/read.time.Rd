\name{read.time.function}

\alias{read.time,orderbook-method}
\alias{read.time}

\title{read.time}
\description{Read the feed data and returns the state of the orderbook at time n.}

\usage{
		read.time(object, n)
}
\arguments{
  \item{object}{ Object of class \code{"orderbook"}}
  \item{n}{ Object of class \code{"numeric"}. The  number of milliseconds 
  after midnight UTC.}
  
} 
\value{
  Return an object of class \code{orderbook}
}
\examples{
# the codes must be run in the same directory with the sample.txt input feed

#ob <- orderbook(feed="sample.txt")
#ob <- read.orders(ob, n=1000)
#ob <- read.time(ob, "09:30:00")
}
\keyword{file}