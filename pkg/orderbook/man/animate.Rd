\name{animate.function}

\alias{animate,orderbook-method}
\alias{load.animation,orderbook-method}
\alias{load.trade.animation,orderbook-method}
\alias{animate}
\alias{load.animation}
\alias{load.trade.animation}

\title{animate}
\description{Animate the orderbook between two times}
\usage{

load.animation(object, from, to, by = "sec", bounds = 0.05)

animate(object, pause = 0.25, type = "sec")
}
\arguments{
  \item{object}{ Object of class \code{"orderbook"}
  }
  \item{from}{ Object of class \code{"character"}. The start time of the
    animation in the form \code{"HH:MM:SS"}.
  }
  \item{to}{ Object of class \code{"character"}. The end time of the
    animation in the form \code{"HH:MM:SS"}.
  }
  \item{by}{ Object of class \code{"character"}. Descides the skip of
    the orderbook.
  }
  \item{bounds}{ Object of class \code{"numeric"}. Percentage above and
    below the midpoint to determine the y limits.
  }
  \item{pause}{ Object of class \code{"numeric"}. Determines how many
      seconds to wait in between printing each Trellis object.
    }
  \item{type}{ Object of class \code{"character"}. Either "msg" or
      "sec", depending on which animation you want to play.
    }
}
\details{loadanimation creates Trellis objects plotting the state of the
  orderbook between from and to, and animate will plot them using a for loop. }
\value{
  Prints many Trellis objects.
}
\examples{
## The examples must be run in the same directory as the sample.txt input file

## ob <- loadanimation(ob, "9:30:00", "9:31:00")
## animate(ob)

}
\keyword{file}