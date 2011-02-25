\name{summary.gantt}

\alias{summary.gantt}
\alias{print.summary.gantt}

\title{Summarize a gantt object}

\description{Summarizes a gantt object.}

\usage{
\method{summary}{gantt}(object, \dots)
\method{print}{summary.gantt}(x, \dots)
}

\arguments{
\item{object}{an object of class \code{gantt}, e.g. as read by
  \code{\link{read.gantt}}.}
\item{x}{an object of class \code{summary.gantt}, as created by
  \code{summary.gantt}.}
\item{\dots}{extra arguments (not used in this version).}
}

\details{Prints a summary of a gantt dataset.}

\value{None.}

\seealso{The \code{gantt} object may be read with \code{\link{read.gantt}}.}

\examples{
library(plan)
data(gantt)
summary(gantt)
}

\references{\url{http://alistair.cockburn.us/crystal/articles/evabc/earnedvalueandburncharts.htm}.}

\author{Dan Kelley}

\keyword{misc}
