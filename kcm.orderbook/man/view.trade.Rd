\name{view.trade.function}

\alias{view.trade,orderbook-method}
\alias{view.trade}

\title{view.trade}
\description{ Read the feed data and returns the state of the orderbook at the time the nth trade occurs.}
\usage{
		view.trade(object, n=1)
}
\arguments{
  \item{object}{ Object of class \code{"orderbook"}}
  \item{n}{ Object of class \code{"numeric"}. The trade index. }    
}
\details{The orderbook object internally keeps track of the position of the trades in the feed data. The view.trade function returns the state of the orderbook at the time of the nth trade. In order to view a trade, the orderbook must already read pass the trade from the input feed.
orders in the }
\value{
  Return an object of class \code{orderbook}
}
\examples{
# the codes must be run in the same directory with the sample.txt input feed

#ob <- orderbook(feed="sample.txt")
#ob <- read.orders(ob, n=1000)
#ob <- view.trade(ob, n=2)
}
\keyword{file}