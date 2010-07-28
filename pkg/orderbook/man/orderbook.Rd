\name{orderbook}
\alias{orderbook}
\title{Creating an Object of Class Backtest}
\description{
  Create an orderbook objects from input data.
}
\usage{

 ob <- orderbook(x      = data.frame(),
                 price  = "price",
                 size   = "size",
                 type   = "type",
	         time   = "time",
	         id     = "id",
	         ask    = "ASK",
                 bid    = "BID",
                 file   = NULL,
                 trader = FALSE)
}

\arguments{
  \item{x}{A data frame containing an orderbook at a moment in time.}
  \item{price}{ Object of class \code{"character"} specifying the name
  of the price column in x.
  }
  \item{size}{ Object of class \code{"character"} specifying
    the name of the size column in x.
  }
  \item{type}{ Object of class \code{"character"} specifying
    the name of the type column in x.
  }
  \item{time}{ Object of class \code{"character"} specifying
    the name of the time column in x.
  }
  \item{id}{ Object of class \code{"character"} specifying
    the name of the id column in x.
  }
  \item{ask}{ Object of class \code{"character"} specifying
    the ASK values for the orderbook.
  }
  \item{bid}{ Object of class \code{"character"} specifying
    the BID values for the orderbook.
  }
  \item{file}{ Object of class \code{"character"} specifying the
    location of the data file.
  }
  \item{trader}{ Object of class \code{"boolean"} specifying whether the
    order book will recognize the user's trades.
  }
}

\details{This function should be used to initially create the orderbook
  function. If you have an order book data.frame from a previous session,
  you can load it and begin from there. If you want to create an empty
  orderbook, just specify the file (see example).}
\value{
  Return an object of class \code{orderbook}
}
\examples{
## ob = orderbook(file = "sample.txt")
}



\keyword{file}