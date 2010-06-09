\name{orderbook}
\alias{orderbook}
\title{Creating an Object of Class Backtest}
\description{
  Create an orderbook objects from input data.
}
\usage{
orderbook(x,
		  price   = "price",
		  size	  = "size",
		  type	  = "type",
		  time	  = "time",
		  id	  = "id",
		  ask	  = "ASK",
		  bid	  = "BID")
}

\arguments{
	\item{x}{A data frame containing the orders data.}	
    \item{price}{ Object of class \code{"character"} specifying 
	  the price values for the orderbook.
	}	
   \item{size}{ Object of class \code{"character"} specifying 
	  the size values for the orderbook.
	}
   \item{type}{ Object of class \code{"character"} specifying 
	  the type values for the orderbook.
	}
   \item{time}{ Object of class \code{"character"} specifying 
	  the time values for the orderbook.
	}
   \item{id}{ Object of class \code{"character"} specifying 
	  the id values for the orderbook.
	}
   \item{ask}{ Object of class \code{"character"} specifying 
	  the ASK values for the orderbook. Optional. Default to "ASK".
	}
   \item{bid}{ Object of class \code{"character"} specifying 
	  the BID values for the orderbook. Optional. Default to "BID".
	}
}

\details{Detail here}
\value{
  Return an object of class \code{orderbook}

  The functions \code{show} and \code{summary} are used to obtain and
  print a short description and longer summary of the results of the
  \code{orderbook}.
}
\keyword{file}