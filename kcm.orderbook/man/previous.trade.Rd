\name{previous.trade.function}

\alias{previous.trade,orderbook-method}
\alias{previous.trade}

\title{previous.trade}
\description{ Read the feed data and returns the state of the orderbook at the time previous trade occurs.}
\usage{
		previous.trade(object)
}
\arguments{
  \item{object}{ Object of class \code{"orderbook"}}
}
\details{The orderbook object internally keeps track of the current position in the orderbook feed. The previous.order function will rewind the feed to the previous trade from the current position.}
\value{
  Return an object of class \code{orderbook}
}
\examples{
# the codes must be run in the same directory with the sample.txt input feed

#ob <- orderbook(feed="sample.txt")
#ob <- read.orders(ob, n=1000)
#ob <- previous.trade(ob)
}
\keyword{file}