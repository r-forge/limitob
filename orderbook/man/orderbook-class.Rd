\name{orderbook-class}
\docType{class}
\alias{orderbook-class}

\alias{show,orderbook-method}
\alias{summary,orderbook-method}
\alias{display,orderbook-method}
\alias{reset,orderbook-method}
\alias{[,orderbook,character,ANY,ANY-method}

% Figured out this last one using promptMethod("[") No idea why it is
% different from the others. Perhaps they should be changed and the
% below deleted?

% All this duplication is hacky! Several of these functions deserve
% their own help files and own test case files. Dumping everything into
% misc.R for tests is hacky. Why do we need to list, say, show() both
% above (where we mention that it is a method) and below (where we
% don't)? Perhaps because this allows a simple ?show to do what we want
% because the R help system is smart?

\alias{show}
\alias{summary}
\alias{display}
\alias{reset}

\title{Class "orderbook"}
\description{Functions for manipulating and extractiong information from
an orderbook}
\section{Objects from the class}{
  Objects can be created by calls to the function \code{orderbook}
}

% The formatting for the Slots is the same that Matrix uses. Still seems
% weird to me.

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

% Again, the format here is the same as in Matrix. Still looks
% weird. Note that Matrix does not show [ as a method.

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "orderbook")}:
      Prints basic information about the \code{orderbook} object.
    }
    \item{summary}{\code{signature(object="orderbook")}: Prints a
      summary of the \code{orderbook} object.
    }
    \item{\code{signature(x = "orderbook", i = "character", j = "ANY", drop = "ANY")}}{
      Prints the orders at that price level.
    }
    \item{"["}{\code{signature(x = "orderbook", i = "character", j = "ANY", drop = "ANY")}:
    }
    \item{display}{\code{signature(object="orderbook")}: Prints by
      default the best 5 price levels and corresponding sizes for the bid
      and ask.
    }
    \item{reset}{\code{signature(object="orderbook")}: Reset the
     orderbook to its initial state.
   }
 }
 }



\examples{
library(orderbook)

filename <- system.file("extdata", "sample.txt", package = "orderbook")

ob <- orderbook(file = filename)
ob <- read.orders(ob, 500)

show(ob)
summary(ob)
display(ob)

% Too short! Show us all the methods. Give us a tour.

}
\keyword{classes}
