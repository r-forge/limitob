\name{animate.function}

\alias{animate,orderbook-method}
\alias{load.animation,orderbook-method}
\alias{load.trade.animation,orderbook-method}
\alias{load.next.trade,orderbook-method}
\alias{load.previous.trade,orderbook-method}

\alias{animate}
\alias{load.animation}
\alias{load.trade.animation}
\alias{load.next.trade}
\alias{load.previous.trade}

\title{animate}
\description{Animate the orderbook between two times}
\usage{
load.animation(object, from, to, fps = 1, by = "sec", bounds = 0.02)

load.trade.animation(object, tradenum, before = 30, after = 30, fps = 1,
                     by = "both", bounds = 0.02)

load.previous.trade(object, before = 30, after = 30, fps = 1, by =
                    "both", bounds = 0.02)

load.next.trade(object, before = 30, after = 30, fps = 1, by = "both",
                bounds = 0.02)

animate(object, by = "sec", start = NULL, end = NULL, pause = 0.25,
        initPause = 2)

}
\arguments{
  \item{object}{ Object of class \code{"orderbook"}
  }
  \item{from}{
    The start time of the animation in the form
    \code{"HH:MM:SS"}.
  }
  \item{to}{
    The end time of the animation in the form
    \code{"HH:MM:SS"}.
  }
  \item{fps}{
    The number of frames per second, i.e. the number of \code{orderbook}
    objects to generate for each second if \code{by = "sec"}.
  }
  \item{by}{
    Either \code{"sec"} or \code{"msg"}.
  }
  \item{bounds}{
    Pennies above (below) the best bid (ask) to display.
  }

  \item{pause}{ Determines how many seconds to wait in between printing
    each animation frame.
  }
  \item{initPause}{ Determines how many seconds to wait before starting
    the animation.
  }
  \item{tradenum}{ Which trade to view the animation for. For example,
    \code{tradenum = 5} will view the trade in the fifth row of
    \code{my.trades}
  }
  \item{before}{The number of messages or seconds before the trade to
    begin animating.}
  \item{after}{The number of messages or seconds after the trade to stop
    animation.}
  \item{start}{ How many frames before the halfway point to start
    at. For example, a start of 5 in a 60 frame animation will start at
    frame 25.}
  \item{end}{How many frames after the halfway point to end at. For
    example, an end of 5 in a 60 frame animation will end at frame 35.}
}
\details{\code{load.animation} is the most basic function for creating
    animations. The user can specify a start and end time and create an
    animation between those two times. The user can also create an
    animation between two row numbers in the input file.

    \code{load.trade.animation} loads trade animations a specified number of
    seconds before or after the trade.

    \code{load.next.trade} uses the \code{trade.index} to load the next
  trade. Users can easily go through trades in order, or use \code{sort}
  to sort the trades and view them in a different
  order. \code{load.previous.trade} decrements the \code{trade.index}.

\code{animate} plays the animation. Only two animations can be stored at
once--one for messages and one for seconds.

  }
\value{
  Prints Trellis objects.
}
\examples{
library(orderbook)
filename <- system.file("extdata", "sample.txt",
                        package = "orderbook")
ob <- orderbook(file = filename)
ob <- load.next.trade(ob)
animate(ob)
}
\keyword{file}