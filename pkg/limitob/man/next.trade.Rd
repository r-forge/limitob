\name{next.trade.function}

\alias{next.trade,orderbook-method}
\alias{next.trade}

\title{next.trade}
\description{ Read the feed data and returns the state of the orderbook at the time next trade occurs.}
\usage{
		next.trade(object)
}
\arguments{
  \item{object}{ Object of class \code{"orderbook"}}
}
\details{The orderbook object internally keeps track of the current position in the orderbook feed. The next.orders function will read and process the feed until the next trade after the current position.
}
\value{
  Return an object of class \code{orderbook}
}
\examples{
# the codes must be run in the same directory with the sample.txt input feed

#ob <- orderbook(feed="sample.txt")
#ob <- read.orders(ob, n=50)
#ob <- next.trade(ob)
}
\keyword{file}