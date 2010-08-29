\name{orderbook-class}
\docType{class}
\alias{orderbook-class}

% Might consider junking all this and just starting again with the
% output of promptClass("orderbook") once we are done messing with the
% orderbook class. Looking at the output of things lik promptMethod("[")
% can also be useful. Also, more test cases!

\alias{[,orderbook,character,ANY,ANY-method}

\title{Class "orderbook"}
\description{Functions for manipulating and extractiong information from
  an orderbook}
\section{Objects from the class}{
  Objects can be created by calls to the function \code{orderbook}
}

% The formatting for the Slots is the same as what promptClass()
% produces, but still seems weird to me.

\section{Slots}{
  \describe{
    \item{\code{current.ob}:}{A \code{data.frame} containing the current state
      of the orderbook.  % Hate this slot name.
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

% Only show methods that are not documented in their own files.

\section{Methods}{
  \describe{
    \item{"["}{\code{signature(x = "orderbook", i = "character", j = "ANY", drop = "ANY")}:
      Prints the orders at that price level.
    }
 }
}


\examples{
library(orderbook)
filename <- system.file("extdata", "sample.txt", package = "orderbook")
ob <- orderbook(file = filename)
ob <- read.messages(ob, 500)

show(ob)
summary(ob)
display(ob)

% Too short! Show us all the methods. Give us a tour.

}
\keyword{classes}
