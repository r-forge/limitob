\name{view.trade.function}

\alias{view.trade,orderbook-method}
\alias{next.trade,orderbook-method}
\alias{previous.trade,orderbook-method}

\alias{previous.trade}
\alias{next.trade}
\alias{view.trade}






\title{view.trade}
\description{View the \code{"orderbook"} object when a particular trade happens.}
\usage{
object = view.trade(object, n=1)

object = next.trade(object)
object = previous.trade(object)
}
\arguments{
  \item{object}{ Object of class \code{"orderbook"}}
  \item{n}{ Object of class \code{"numeric"}. The trade index. }
}
\details{The orderbook object internally keeps track of the position of
the trades in the data feed. The view.trade function returns the state
of the orderbook at the time of the nth trade. This function can only view trades
that occurred prior to the current order.book time.

Previous.trade shows the state of the orderbook at the first trade to
have happened before the current time. Next.trade shows the state of the
orderbook at the very next trade to have happened after the current time. }
\value{
  Return an object of class \code{orderbook}
}
\examples{
## The examples must be run in the same directory as the sample.txt input feed

## ob <- orderbook(feed="sample.txt")
## ob <- read.orders(ob, n=1000)
## ob <- view.trade(ob, n=2)
}
\keyword{file}