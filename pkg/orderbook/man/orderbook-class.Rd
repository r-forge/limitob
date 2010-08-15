\name{orderbook-class}
\docType{class}
\alias{orderbook-class}

\alias{show,orderbook-method}
\alias{summary,orderbook-method}
\alias{best.bid,orderbook-method}
\alias{best.ask,orderbook-method}
\alias{bid.price.levels,orderbook-method}
\alias{ask.price.levels,orderbook-method}
\alias{total.price.levels,orderbook-method}
\alias{bid.orders,orderbook-method}
\alias{ask.orders,orderbook-method}
\alias{total.orders,orderbook-method}
\alias{mid.point,orderbook-method}
\alias{inside.market,orderbook-method}
\alias{spread,orderbook-method}
\alias{display,orderbook-method}
\alias{reset,orderbook-method}
\alias{[,orderbook,character,ANY,ANY-method}

\alias{bid.price.levels}
\alias{ask.price.levels}
\alias{total.price.levels}
\alias{bid.orders}
\alias{ask.orders}
\alias{total.orders}
\alias{mid.point}
\alias{inside.market}
\alias{spread}
\alias{display}
\alias{best.ask}
\alias{best.bid}
\alias{reset}
\alias{[}

\title{Class "orderbook"}
\description{Functions for manipulating and extractiong information from
an orderbook}
\section{Objects from the class}{
  Objects can be created by calls to the function \code{orderbook}
}

\section{Slots}{
  \describe{
    \item{\code{current.ob}:}{A \code{data.frame} containing the current state
      of the orderbook.
    }
    \item{\code{current.time}:}{A \code{numeric} in milliseconds after
      midnight that indicates the time of \code{current.ob}.
    }
    \item{\code{file}:}{A \code{character} which specifies the
      location of the file containing the data file.
    }
    \item{\code{file.index}:}{An \code{numeric} that indicates the
      current row number of the input file.
    }
    \item{\code{trade.data}:}{A \code{data.frame} that contains data on all
      trades that occurred before current.time.
    }
    \item{\code{my.trades}:}{A \code{vector} that contains the file row
      numbers of the users trades.
    }
    \item{\code{animation}:}{A \code{list} that contains the
      location of the last created message and time animations.
    }
    \item{\code{trader}:}{A \code{boolean} that indicates whether or not
      the order book will recognize the user's trades.
    }
    \item{\code{trade.index}:}{A \code{numeric} that keeps track of the
      last viewed trade animation.}
  }
}


\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "orderbook")}:
      Prints basic information about the \code{orderbook} object.
    }
    \item{best.bid}{\code{signature(object = "orderbook")}: Returns the
      current best bid.
    }
    \item{best.ask}{\code{signature(object = "orderbook")}: Returns the
      current best ask.
    }
    \item{summary}{\code{signature(object="orderbook")}: Prints a
      summary of the \code{orderbook} object.
    }
    \item{[}{\code{signature(x = "orderbook", i = "character")}: Prints
      the orders at that price level.
    }
    \item{display}{\code{signature(object="orderbook")}: Prints by
      default the best 5 price levels and corresponding sizes for the bid
      and ask.
    }
    \item{bid.price.levels}{\code{signature(object="orderbook")}: Returns
      the number of bid price levels.
    }
    \item{ask.price.levels}{\code{signature(object="orderbook")}: Returns
      the number of ask price levels.
    }
    \item{total.price.levels}{\code{signature(object="orderbook")}:
      Returns the total number of price levels.
    }
    \item{bid.orders}{\code{signature(object="orderbook")}: Returns the
      number of bids.
   }
   \item{ask.orders}{\code{signature(object="orderbook")}: Returns the
     number of asks.
   }
   \item{total.orders}{\code{signature(object="orderbook")}: Returns the
     total number of orders.
   }
   \item{mid.point}{\code{signature(object="orderbook")}: Returns the
     midpoint between the best ask and the  best bid.
   }
   \item{inside.market}{\code{signature(object="orderbook")}:  Returns a
     data frame with a row for the best ask and a row for the best bid. The
     columns are price, size, and type.
   }
   \item{spread}{\code{signature(object="orderbook")}: Returns the
     spread between the best ask and best bid .
   }

   \item{reset}{\code{signature(object="orderbook")}: Reset the
     orderbook to its initial state.
   }
 }
 }



\examples{
library(orderbook)

filename <- system.file("data", "sample.txt",
                        package = "orderbook")

ob <- orderbook(file = filename)
ob <- read.orders(ob, 500)

show(ob)
summary(ob)
display(ob)
}
\keyword{classes}