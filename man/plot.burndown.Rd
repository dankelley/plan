\name{plot.burndown}

\alias{plot.burndown}

\title{Draw a burndown chart}

\description{Plot a burndown object.}

\usage{plot.burndown(x, col=NULL,
	draw.plan=TRUE, draw.regression=TRUE, draw.lastupdate=FALSE,
	t.stop="", 
	y.name="Remaining Effort",
	debug=FALSE, ...)
}

\arguments{
  \item{x}{an object of class \code{burndown}, to be plotted.}
  \item{col}{list of colours for items, starting with the first key in
    the file (which will be at the bottom of the chart).  If not
    specified, the \code{\link{hcl}} scheme will be used, to generate
    colours that are distinct, that show up reasonably well on a
    monitor.}
  \item{draw.plan}{boolean, set to \code{TRUE} to draw the plan, as a
    descending line with a horizontal intercept.}
  \item{draw.regression}{boolean, set to \code{TRUE} to draw a
    regression line of actual overall progress.}
  \item{draw.lastupdate}{boolean, set to \code{TRUE} to draw the last
    update (which otherwise requires a sharp eye).}
  \item{t.stop}{a POSIX time, the maximum time for graph (defaults to
    deadline if not given).}
  \item{y.name}{character string, for labelling the vertical axis.}
  \item{debug}{boolean, set to \code{TRUE} to monitor the work.}
  \item{...}{extra things handed down to plotting functions.}
}

\details{Plots a burndown chart.}

\value{The object, returned invisibly.}

\seealso{\code{\link{read.burndown}} and \code{\link{summary.burndown}}.}

\examples{
library(plan)
data(burndown)
summary(burndown)
plot(burndown)
}

\references{
\url{http://alistair.cockburn.us/crystal/articles/evabc/earnedvalueandburncharts.htm}.
}

\author{Dan Kelley}

\keyword{misc}
