\name{plot.gantt}

\alias{plot.gantt}

\title{Draw a Gantt diagram}

\description{Plot a Gantt object.}

\usage{\method{plot}{gantt}(x,
           xlim,
           time.format = NULL,
           time.labels.by, time.lines.by,
	   event.time = NULL, event.label = NULL, event.side=3,
	   col.done = gray(0.3), col.notdone = gray(0.9),
	   col.event = gray(0.1), col.connector = "black",
           grid.col = "lightgray", grid.lty = "dotted",
	   main = "",
	   cex=par("cex"), 
	   debug=FALSE,
 ...)
}
\arguments{
  \item{x}{an object of class \code{gantt}.}
  \item{xlim}{optional range of time axis; if not provided, the range of times
      in \code{x} will be used.}
  \item{time.format}{format for dates on time axis; defaults to 3-letter month.}
  \item{time.labels.by}{suggested label increment on time axis,
    e.g. \code{time.labels.by="2 months"} to get a two-month interval.
    If not supplied, the axis will be generated automatically.}
  \item{time.lines.by}{suggested interval between vertical grid lines on
    the plot, e.g. \code{time.lines.by="1 week"} for weekly.  If not
    supplied, the grid will be generated automatically.}
  \item{event.time}{List of events, e.g. conferences, whose time cannot
    be altered.}
  \item{event.label}{list of names of these events.}
  \item{event.side}{side for event labels.}
  \item{col.done}{colour of work that has been done already.}
  \item{col.notdone}{colour of work that has not been done already.}
  \item{col.event}{colour of events.}
  \item{col.connector}{colour of (optional) connectors between items.}
  \item{grid.col}{colour for grid}
  \item{grid.lty}{line type for grid}
  \item{main}{character string to be used as chart title.}
  \item{cex}{numeric, font-size factor.}
  \item{debug}{boolean, set to \code{TRUE} to monitor the work.}
  \item{...}{extra things handed down.}
}

\details{Plots a gantt chart, possibly with events superimposed.}

\value{The gantt object, returned invisibly.}

\note{The defaults work well for projects that take a year or
  two. Consider adjusting \code{time.labels.by} and \code{time.lines.by}
  for projects that are much shorter or longer.}

\seealso{Use \code{\link{read.gantt}} to read gantt data, and 
\code{\link{summary.gantt}} to summarize them.
}

\examples{
library(plan)
data(gantt)
summary(gantt)
plot(gantt)
# Add a couple of event
event.label <- c("Proposal", "AGU")
event.time <- c("2008-01-28", "2008-12-10")
plot(gantt, event.label=event.label,event.time=event.time)
}

\references{Gantt diagrams are described on wikipedia
  \url{http://en.wikipedia.org/wiki/Gantt_Chart}.}

\author{Dan Kelley}

\keyword{misc}
