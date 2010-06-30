\name{animate.function}

\alias{animate,character-method}
\alias{preload,orderbook-method}

\alias{animate}
\alias{preload}

\title{animate}
\description{Animate the orderbook between two times}
\usage{
preload(object, from, to, by = "sec", bounds = 0.05, type = "n", file)

animate(file)
}
\arguments{
  \item{object}{ Object of class \code{"orderbook"}}
  \item{from}{ Object of class \code{"character"}. The start time of the
    animation in the form \code{"HH:MM:SS"}.}
  \item{to}{ Object of class \code{"character"}. The end time of the
    animation in the form \code{"HH:MM:SS"}.}
  \item{by}{ Object of class \code{"character"}. Descides the skip of
    the orderbook.}
  \item{bounds}{ Object of class \code{"numeric"}. Percentage above and
    below the midpoint to determine the y limits.}
  \item{type}{ Object of class \code{"character"}. Either \code{"n"},
    \code{"s"}, or \code{"c"}.}
  \item{file}{ Object of class \code{"character"}. This is the filename
    for the \code{".Rdata"} file you create using \code{"preload"}, and
    the filename of the file you want to load with \code{"animate"}.}
}
\details{Preload creates Trellis objects plotting the state of the
  orderbook between from and to, and animate will plot them using a for loop. }
\value{
  Prints many Trellis objects.
}
\examples{
## The examples must be run in the same directory as the sample.txt input file

## preload(file = "file.Rdata", a, "9:30:00", "9:31:00")
## animate("file.Rdata")

}
\keyword{file}