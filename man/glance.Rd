\name{glance}
\alias{glance}
\alias{glance.didbjs}
\title{Glance at a didbjs result}
\usage{
glance(x, ...)
\method{glance}{didbjs}(x, ...)
}
\arguments{
\item{x}{A \code{didbjs} result.}
\item{...}{Unused.}
}
\description{
Return a one-row summary of a \code{didbjs} result, including term counts,
sample counts, artifact/pretrend flags, and the result object version.
}
\value{
A one-row data frame.
}
