#' Class to store burndown objects
setClass("burndown", contains="plan")

#' Draw a burndown chart
#' 
#' Plot a burndown object.
#' 
#' Plots a burndown chart.
#' 
#' @param x an object of class \code{burndown}, to be plotted.
#' @param col list of colours for items, starting with the first key in the
#' file (which will be at the bottom of the chart).  If not specified, the
#' \code{\link{hcl}} scheme will be used, to generate colours that are
#' distinct, that show up reasonably well on a monitor.
#' @param draw.plan boolean, set to \code{TRUE} to draw the plan, as a
#' descending line with a horizontal intercept.
#' @param draw.regression boolean, set to \code{TRUE} to draw a regression line
#' of actual overall progress.
#' @param draw.lastupdate boolean, set to \code{TRUE} to draw the last update
#' (which otherwise requires a sharp eye).
#' @param t.stop a POSIX time, the maximum time for graph (defaults to deadline
#' if not given).
#' @param y.name character string, for labelling the vertical axis.
#' @param debug boolean, set to \code{TRUE} to monitor the work.
#' @param ... extra things handed down to plotting functions.
#' @return The object, returned invisibly.
#' @author Dan Kelley
#' @family things related to \code{burndown} data
#' @references
#' \url{http://alistair.cockburn.us/crystal/articles/evabc/earnedvalueandburncharts.htm}.
#' @examples
#' 
#' library(plan)
#' data(burndown)
#' summary(burndown)
#' plot(burndown)
#' 
setMethod(f="plot",
          signature=signature("burndown"),
          definition=function(x, col=NULL, draw.plan=TRUE,
                          draw.regression=TRUE,
                          draw.lastupdate=FALSE,
                          t.stop="",
                          y.name="Remaining Effort",
                          debug=FALSE, ...) {
              opar <- par(no.readonly = TRUE)
              on.exit(opar)
              mgp <- c(2, 3/4, 0)
              par(mgp=mgp)
              par(mar=c(mgp[1]+1, mgp[1]+1,1,1))
              num.items = length(x[["tasks"]]$key)
              num.progress = length(x[["progress"]]$key)
              if (is.null(col)) {
                  ##col <- heat.colors(num.items)
                  col <- hcl(h = 360*(1:num.items)/num.items, c=70,l=80)
              }
              if (debug)
                  cat("Progress:\n")
              t <- x[["start"]]
              effort.remaining <<- x[["tasks"]]$effort
              e <- effort.remaining
              if (debug) {
                  cat("TIME:");print(t);cat("\n")
                  cat("effort remaining:\n");print(effort.remaining);cat("\n")
                  cat(sprintf("    %5s\t%20s\t%15s\n","Key","Percent Complete","Time"))
              }
              num.progress = length(x[["progress"]]$key)
              for (i in 1:num.progress) {
                  if (debug) {
                      cat(sprintf("    %5s\t%20s   ", x[["progress"]]$key[i], x[["progress"]]$progress[i]))
                      cat(format(x[["progress"]]$time[i]))
                      cat("\n")
                  }
                  t <- c(t, x[["progress"]]$time[i])
                  k <- x[["progress"]]$key[i]
                  effort.remaining[k] <- x[["tasks"]]$effort[k] * (1 - x[["progress"]]$progress[i]/100)
                  if (debug) {
                      cat(paste("k=",k,"\n"))
                      cat("TIME:\n");print(x[["progress"]]$time[i]);cat("\n")
                      cat("effort remaining:\n");print(effort.remaining);cat("\n")
                  }
                  e <- c(e,effort.remaining)
              }
              e.matrix <- matrix(e,ncol=num.items,byrow=TRUE)
              if (t.stop != "") {
                  time.max = as.POSIXct(t.stop)
              } else {
                  time.max = x[["deadline"]]
              }
              time.range <- range(c(t[1], time.max))
              plot(time.range, range(c(0,sum(x[["tasks"]]$effort))),type='n',
                   ylab=y.name,
                   xaxs="i")
              xx <- c(t, rev(t))
              bottom <- rep(0,1+num.progress)
              for (i in 1:num.items) {
                  y <- e.matrix[,i] + bottom
                  yy <- c(y, rev(bottom))
                  bottom <- y
                  polygon(xx,yy,col=col[i])
              }
              ## Indicate prediction (possibly with a regression line)
              total.effort <- c();
              for (i in 1:dim(e.matrix)[1])
                  total.effort <- c(total.effort,sum(e.matrix[i,]))
              effort.anomaly <- total.effort - total.effort[1]
              t.anomaly <- t - t[1]
              m <- lm (effort.anomaly ~ t.anomaly - 1)
              slope <- m$coefficients[1][[1]]
              intercept <- total.effort[1] - slope * as.numeric(t[1])
              t.done <- floor(-intercept / slope)
              if (draw.regression)
                  abline(a=intercept, b=slope, col="red",lwd=2,lty=2)
              class(t.done) <- "POSIXct"
              ##cat(paste("NOTE: predicted time of completion is", format(t.done)))
              ## Indicate plan
              if (draw.plan) {
                  lines(c(t[1],x[["deadline"]]),c(sum(x[["tasks"]]$effort),0),col="red",lwd=3)
                  abline(h=0,col="red",lwd=3)
              }
              final.effort <-  sum(e.matrix[dim(e.matrix)[1],])
              if (draw.lastupdate) {
                  points(t[length(t)],final.effort,col="yellow",cex=2.5,pch=19)
                  points(t[length(t)],final.effort,col="blue",cex=2.6)
                  ##lines(c(t[length(t)],time.max),rep(final.effort,2),col=gray(0.9),lwd=3)#,col="red",lwd=3)
              }
              ## legend
              cex <- if (length(x[["task"]]$description) < 5) 1 else 4/5
              legend("topright",legend=rev(x[["task"]]$description),fill=rev(col),cex=cex,y.intersp=1.5*cex)
              mtext(paste(paste(format(time.range), collapse=" to "),
                          attr(x$data$ts$time[1], "tzone")),
                    side=3, cex=cex, adj=0)
              invisible(x)
          })





#' Scan burndown data file
#' 
#' Read a data file containing burndown information.
#' 
#' Reads a \code{burndown} dataset.
#' 
#' A strict format is required, in which the following items must be present,
#' in the stated order, and with nothing else in the file.  An example is given
#' after the description.
#' 
#' \itemize{
#' 
#' \item Line 1: contains two comma-separated items: the string \code{Start},
#' and a time expressed in ISO 8601 format (\code{YYYY-MM-DD} or
#' \code{YYY-MM-DD hh:mm:ss}).  This line indicates the start of the project.
#' 
#' \item Line 2: as Line 1, but the string is to be \code{Start}, and the line
#' indicates the deadline for the project.
#' 
#' \item Line 3: a header line for a "task" list, comprising the following
#' three words separated by commas: \code{Key}, \code{Description}, and
#' \code{Effort}.
#' 
#' \item Lines 4 to N: data lines, each containing three items: a numeric index
#' "Key" for the task, a short "Description" of the task, and the estimated
#' "Effort" for this task, expressed as a number. The keys must be distinct,
#' and they must match the keys in the progress table (see below).  The
#' description should be short enough to give a reasonable-size legend as
#' created by \code{\link{plot.burndown}}.  The effort may be expressed in any
#' convenient unit, e.g. the number of hours or days for the task, or as a
#' percentage of the overall task.
#' 
#' \item Line N+1: a header line for the "Progress" list, comprising the
#' following four words separated by commas: \code{Key}, \code{Done}, and
#' \code{Time}.
#' 
#' \item Line N+2 to end: data lines holding Progress items. Each "Key" must
#' match a key in the task list.  The "Done" column holds the percentage of the
#' task that has been completed. The "Time" is in ISO 8601 format, as described
#' above.  }
#' 
#' @section Sample data file:
#' \preformatted{
#' FILL IN
#' }
#' 
#' @param file a connection or a character string giving the name of the file
#' to load.
#' @param debug boolean, set to \code{TRUE} to print debugging information.
#' @return A burndown object.
#' @author Dan Kelley
#' @family things related to \code{burndown} data
#' @examples
#' 
#' \dontrun{
#' library(plan)
#' b <- read.burndown("burndown.dat")
#' summary(b)
#' plot(b)
#' }
#' 
read.burndown <- function(file, debug=FALSE)
{
    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    quiet <- !debug
    ## Start, ISdate
    tokens <- trim.whitespace(scan(file, what='char', sep=",", nlines=1,quiet=quiet,blank.lines.skip=TRUE))
    name <- tokens[1]
    if (name != "Start") stop("First line of file must be 'Start' followed by an ISO date but got '",
        paste(tokens, collapse=","))
    start <- as.POSIXct(tokens[2])
    ## Deadline, ISOdate
    tokens <- trim.whitespace(scan(file,what='char',sep=",",nlines=1,quiet=quiet,blank.lines.skip=TRUE))
    name <- tokens[1]
    deadline <- as.POSIXct(tokens[2])
    if (name != "Deadline") stop("Second line of file must be 'Deadline' followed by an ISO date, but got '",
        paste(tokens, collapse=","), "'")
    ## Header
    tokens <- trim.whitespace(scan(file,what='char',sep=',',nlines=1,quiet=quiet,blank.lines.skip=TRUE))
    check.tokens(tokens, c("Key", "Description", "Effort"))
    task.key <- c()
    task.description <- c()
    task.effort <- c()
    while (TRUE) { # TASK: key description effort
        tokens <- trim.whitespace(scan(file, what=character(0),nlines=1,blank.lines.skip=FALSE,quiet=quiet,sep=","))
        if (tokens[1] == "Key")
            break
        if (3 == length(tokens)) {
            task.key         <- c(task.key,         as.numeric(tokens[1]))
            task.description <- c(task.description, tokens[2])
            task.effort      <- c(task.effort,      as.numeric(tokens[3]))
        }
    }
    ## "Key,	Progress, Time", followed by data lines
    check.tokens(tokens, c("Key", "Done", "Time"))
    progress.key <- progress.done <- progress.time <- NULL
    while (TRUE) {
        tokens <- trim.whitespace(scan(file, what=character(0),nlines=1,blank.lines.skip=FALSE,quiet=quiet, sep=","))
        message("below is tokens:")
        print(tokens)
        if (is.na(tokens[1]))
            break
        key <- as.numeric(tokens[1])
        message("key: ", key)
        message("below is task.key:")
        print(task.key)
        if (!(key %in% task.key)) {
            msg <- paste("Progress key",key,"not in the list of task keys\n\tOffending line in data file follows\n\t",tokens[1]," ",tokens[2], " ", tokens[3])
            stop(msg)
        }
        done <- as.numeric(tokens[2])
        time <- as.POSIXct(tokens[3])
        progress.key     <- c(progress.key,     key)
        progress.done    <- c(progress.done,    done)
        progress.time    <- c(progress.time,    time)
    }
    class(progress.time) <- "POSIXct"
    ## BUG: should ensure item is in task
    o <- order(progress.time)
    progress.key     <- progress.key[o]
    progress.done    <- progress.done[o]
    progress.time    <- progress.time[o]
    rval <- new("burndown")
    rval@data <- l <- list(start=start,
                           deadline=deadline,
                           tasks=list(key=task.key,
                                      description=task.description,
                                      effort=task.effort),
                           progress = list(key=progress.key,
                                           progress=progress.done,
                                           time=progress.time)
                 )
    rval
}





#' Summarize a burndown object
#' 
#' Summarizes a burndown object.
#' 
#' Prints a summary of a burndown dataset.
#' 
#' @aliases summary.burndown print.summary.burndown
#' @param object an object of class \code{burndown}, e.g. as read by
#' \code{\link{read.burndown}}.
#' @param x an object of class \code{summary.burndown}, as created by
#' \code{summary.burndown}.
#' @param \dots extra arguments (not used in this version).
#' @return None.
#' @author Dan Kelley
#' @family things related to \code{burndown} data
#' @examples
#' 
#' library(plan)
#' data(burndown)
#' summary(burndown)
#' 
setMethod(f="summary",
          signature="burndown",
          definition=function(object, ...) {
              cat(paste("Start,   ", format(x[["start"]])), "\n")
              cat(paste("Deadline,", format(x[["deadline"]])), "\n")
              num.tasks = length(x[["tasks"]]$key)
              dspace <- max(nchar(x[["tasks"]]$description))
              cat(sprintf("Key, Description,%s %5s\n",
                          paste(rep(" ", dspace - nchar("Description")), collapse=""),
                          "Effort"))
              for (i in 1:num.tasks) {
                  space <- paste(rep(" ", dspace - nchar(x[["tasks"]]$description[i])), collapse="")
                  cat(sprintf("%3s, %s,%s %s\n",
                              x[["tasks"]]$key[i], x[["tasks"]]$description[i], space, x[["tasks"]]$effort[i]))
              }
              cat("Key, Done,  Time\n")
              num.progress = length(x[["progress"]]$key)
              for (i in 1:num.progress) {
                  cat(sprintf("%3s, %5s, ", x[["progress"]]$key[i], x[["progress"]]$progress[i]))
                  cat(format((x[["progress"]]$time[i])))
                  cat("\n")
              }
              invisible()
          })
