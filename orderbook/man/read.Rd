\name{read.functions}

\alias{read.messages}
\alias{read.time}

\title{read.order}
\description{Read messages into the object.}
\usage{
read.messages(object, n)
read.time(object, time)
}
\arguments{
  \item{object}{Object of class \code{orderbook}}
  \item{n}{An integer.}
  \item{time}{A time in HH:MM:SS format.}
}
\details{The orderbook keeps track of its current position within the
  data file. The \code{read.messages} function will read and
  process the next n messages from the input file. The \code{read.time}
  function will process all messsages in the input file before the
  specified time.
}
\value{
  Return an object of class \code{orderbook}.
}
\examples{
library(orderbook)
filename <- system.file("extdata", "sample.txt", package = "orderbook")
ob <- orderbook(file = filename)
ob <- read.messages(ob, 100)
ob <- read.time(ob, "9:30:00")
}
\keyword{file}
