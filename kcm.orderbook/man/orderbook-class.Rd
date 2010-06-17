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
\alias{get.order.info,orderbook-method}
\alias{total.orders,orderbook-method}
\alias{mid.point,orderbook-method}
\alias{inside.market,orderbook-method}
\alias{spread,orderbook-method}
\alias{snapshot,orderbook-method}
\alias{add.order,orderbook-method}
\alias{remove.order,orderbook-method}
\alias{replace.order,orderbook-method}
\alias{market.order,orderbook-method}
\alias{display,orderbook-method}
\alias{plot,orderbook-method}

\alias{bid.price.levels}
\alias{ask.price.levels}
\alias{total.price.levels}
\alias{bid.orders}
\alias{ask.orders}
\alias{total.orders}
\alias{mid.point}
\alias{get.order.info}
\alias{inside.market}
\alias{spread}
\alias{snapshot}
\alias{add.order}
\alias{remove.order}
\alias{replace.order}
\alias{market.order}
\alias{display}
\alias{plot}
\alias{best.ask}
\alias{best.bid}

\title{Class "orderbook"}
\description{Contains representation of an orderbook}
\section{Objects from the Class}{
  Objects can be created by calls to the function \code{orderbook}						 
}

\section{Slots}{
  \describe{
    \item{\code{current.ob}:}{A data frame containing the orders of this orderbook.
    }
    \item{\code{current.time}:}{A numeric value that indicates the time of this orderbook.
    }
    \item{\code{ob.names}:}{A character vector which indicates the names of the columns
      in  \code{current.ob} to be used as input variables.
    }
    \item{\code{feed}:}{A character string which specifies the
      location of the file containing formatted orders.
    }
    \item{\code{feed.index}:}{A numeric value that indicates the
      current position of the feed. Basically, this is the row number of the
      input feed.
    }
    \item{\code{ob.data}:}{A matrix that contains all order information
      for manipulation of the orderbook.
    }
    \item{\code{current.pos}:}{A numeric value which indicates the
      current position in the orderbook
    }
    \item{\code{trade.data}:}{A hash table that contains the position of
      the trade in the feed input. This helps the next.trade function to
      quickly calculate the row number of trade order.
    }
    \item{\code{trade.index}:}{A numeric value that indicates the index
      of the current looked trade. This help the next.trade function to
      figure out where to jump to when it is called.
    }
    \item{\code{ids}:}{A hash table that contains the (id,
      row_number_in_the_feed) of the orders.
    }
  }
}

\section{Methods}{
  \describe{
   \item{show}{\code{signature(object = "orderbook")}: Prints the variables 
     used in the orderbook
   }    
   \item{summary}{\code{signature(object = "orderbook")}: Prints the summary 
     of the orderbook
   }  
   \item{best.bid}{\code{signature(object = "orderbook")}: Returns the current best bid
   }
   \item{best.ask}{\code{signature(object = "orderbook")}: Returns the current best ask     
   }
   \item{summary}{\code{signature(object="orderbook")}: Prints the summary 
     of the orderbook
   }
   \item{get.order.info}{\code{signature(object = "orderbook")}: Returns the price
        and size of the order with specified ID.
   }
   \item{plot}{\code{signature(x = "orderbook")}: Plots the orderbook
   }
   \item{display}{\code{signature(object="orderbook")}: Prints the state of the orderbook.
     The results is similar to \code{plot}      
   }
   \item{add.order}{\code{signature(object="orderbook")}: Adds an order to this orderbook.
     Returns an oderbook object with updated information.     
   }         
   \item{remove.order}{\code{signature(object="orderbook")}: Removes an order from this 
     orderbook. Returns an oderbook object with updated information.     
   }
   \item{replace.order}{\code{signature(object="orderbook")}: Replaces an order in this 
     orderbook. Returns an oderbook object with updated information.     
   }
   \item{market.order}{\code{signature(object="orderbook")}: Insert a market order to this
     orderbook. Returns an oderbook object with updated information.     
   }
   \item{bid.price.levels}{\code{signature(object="orderbook")}: Returns the number of bid
     price levels.     
   }
   \item{ask.price.levels}{\code{signature(object="orderbook")}: Returns the number of ask
     price levels.     
   }
   \item{total.price.levels}{\code{signature(object="orderbook")}: Returns the total number
     of price levels.     
   }
   \item{bid.orders}{\code{signature(object="orderbook")}: Returns the number of bids.     
   } 
   \item{ask.orders}{\code{signature(object="orderbook")}: Returns the number of asks.     
   } 
   \item{total.orders}{\code{signature(object="orderbook")}: Returns the total number of
     orders.     
   } 
   \item{mid.point}{\code{signature(object="orderbook")}: Returns the midpoint between
     best ask price and best bid price.     
   }
   \item{inside.market}{\code{signature(object="orderbook")}:  Returns a data frame with 
     a row for the best ask and a row for the best bid. The columns are price, size, and type.
   }
   \item{spread}{\code{signature(object="orderbook")}: Returns the spread between best ask
     price and best bid price.
   }
   \item{snapshot}{\code{signature(object="orderbook")}: Returns the snapshot of the
     orderbook at a point in time.
   }
  }
}

\examples{

data(sample)

summary(ob)

plot(ob)

}
\keyword{classes}