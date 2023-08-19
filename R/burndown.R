#' Class to store burndown objects
#' @family things related to burndown data
setClass("burndown", contains="plan")

setMethod(f="initialize",
    signature="burndown",
    definition=function(.Object)
    {
        return(.Object)
    })

#' Draw a burndown chart
#'
#' Plot a burndown chart.
#'
#' @param x A [burndown-class] object.
#'
#' @param col list of colours for items, starting with the first key in the
#' file (which will be at the bottom of the chart).  If not specified, the
#' [hcl()] scheme will be used, to generate colours that are
#' distinct, that show up reasonably well on a monitor.
#'
#' @param draw.plan boolean, set to `TRUE` to draw the plan, as a
#' blue descending line with a horizontal intercept.
#'
#' @param draw.regression boolean, set to `TRUE` to draw a red dashed
#' line indicating the overall progress, as determined by regression.
#'
#' @param draw.lastupdate boolean, set to `TRUE` to draw the last update
#' (which otherwise requires a sharp eye).
#'
#' @param t.stop a POSIX time, the maximum time for graph (defaults to deadline
#' if not given).
#'
#' @param y.name character string, for labelling the vertical axis.
#'
#' @param debug boolean, set to `TRUE` to monitor the work.
#'
#' @param ... extra things handed down to plotting functions.
#'
#' @author Dan Kelley
#'
#' @family things related to burndown data
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Burndown_chart}
#'
#' @examples
#' library(plan)
#' data(burndown)
#' summary(burndown)
#' plot(burndown)
#' @aliases plot.burndown
#' @export
#' @importFrom stats lm
setMethod(f="plot",
    signature=signature("burndown"),
    definition=function(x, col=NULL, draw.plan=TRUE,
        draw.regression=TRUE,
        draw.lastupdate=FALSE,
        t.stop="",
        y.name="Remaining Effort", debug=FALSE, ...)
    {
        opar <- par(no.readonly = TRUE)
        on.exit(opar)
        num.items <- length(x[["tasks"]]$key)
        num.progress <- length(x[["progress"]]$key)
        if (is.null(col)) {
            #col <- heat.colors(num.items)
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
        num.progress <- length(x[["progress"]]$key)
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
        if (debug)
            cat("BEFORE t.stop='", format(t.stop), "' (class ",
                paste(class(t.stop), collapse=","), ")\n", sep="")
        time.max <- if (inherits(t.stop, "POSIXt")) {
            t.stop
        } else if (is.character(t.stop) && t.stop[1] != "") {
            as.POSIXct(t.stop[1])
        } else {
            x[["deadline"]][1]
        }
        if (debug)
            cat("AFTER time.max='", format(time.max), "'\n", sep="")
        time.range <- range(c(t[1], time.max))
        plot(time.range, range(c(0,sum(x[["tasks"]]$effort))),type='n',
            xlab="", ylab=y.name,
            xaxs="i")
        xx <- c(t, rev(t))
        bottom <- rep(0,1+num.progress)
        for (i in 1:num.items) {
            y <- e.matrix[,i] + bottom
            yy <- c(y, rev(bottom))
            bottom <- y
            polygon(xx,yy,col=col[i])
        }
        # Indicate prediction (possibly with a regression line)
        totalEffort <- c();
        for (i in 1:dim(e.matrix)[1])
            totalEffort <- c(totalEffort,sum(e.matrix[i,]))
        effortAnomaly <- totalEffort - totalEffort[1]
        tAnomaly <- t - t[1]
        m <- lm(effortAnomaly ~ tAnomaly - 1)
        slope <- m$coefficients[1][[1]]
        intercept <- totalEffort[1] - slope * as.numeric(t[1])
        #t.done <- floor(-intercept / slope)
        if (draw.regression)
            abline(a=intercept, b=slope, col=2,lwd=2,lty=2)
        #class(t.done) <- "POSIXct"
        #cat(paste("NOTE: predicted time of completion is", format(t.done)))
        # Indicate plan
        if (draw.plan) {
            lines(c(t[1],x[["deadline"]]),c(sum(x[["tasks"]]$effort),0),col=4,lwd=3)
            abline(h=0,col=4,lwd=1)
        }
        final.effort <-  sum(e.matrix[dim(e.matrix)[1],])
        if (draw.lastupdate) {
            points(t[length(t)],final.effort,col="yellow",cex=2.5,pch=19)
            points(t[length(t)],final.effort,col="blue",cex=2.6)
            #lines(c(t[length(t)],time.max),rep(final.effort,2),col=gray(0.9),lwd=3)#,col="red",lwd=3)
        }
        # legend
        cex <- if (length(x[["tasks"]]$description) < 5) 1 else 4/5
        legend("topright",legend=rev(x[["tasks"]]$description),fill=rev(col),cex=cex,y.intersp=1.5*cex)
        mtext(paste(paste(format(time.range), collapse=" to "),
                attr(x[["ts"]]$time[1], "tzone")),
            side=3, cex=cex, adj=0)
        invisible(x)
    })


#' Scan burndown data file
#'
#' Read a data file containing burndown information.
#'
#' Reads a burndown dataset.
#'
#' A strict format is required, in which the following items must be present,
#' in the stated order, and with nothing else in the file.  An example is given
#' after the description.
#'
#' * Line 1: contains two comma-separated items: the string `Start`,
#' and a time expressed in ISO 8601 format (`YYYY-MM-DD` or
#' `YYY-MM-DD hh:mm:ss`).  This line indicates the start of the project.
#'
#' * Line 2: as Line 1, but the string is to be `Start`, and the line
#' indicates the deadline for the project.
#'
#' * Line 3: a header line for a "tasks" list, comprising the following
#' three words separated by commas: `Key`, `Description`, and
#' `Effort`.
#'
#' * Lines 4 to N: data lines, each containing three items: a numeric index
#' "Key" for the task, a short "Description" of the task, and the estimated
#' "Effort" for this task, expressed as a number. The keys must be distinct,
#' and they must match the keys in the progress table (see below).  The
#' description should be short enough to give a reasonable-size legend as
#' created by [plot,burndown-method()].  The effort may be expressed in any
#' convenient unit, e.g. the number of hours or days for the task, or as a
#' percentage of the overall task.
#'
#' * Line N+1: a header line for the "Progress" list, comprising the
#' following four words separated by commas: `Key`, `Done`, and
#' `Time`.
#'
#' * Line N+2 to end: data lines holding Progress items. Each "Key" must
#' match a key in the task list.  The "Done" column holds the percentage of the
#' task that has been completed. The "Time" is in ISO 8601 format, as described
#' above.
#'
#' @section Sample data file:
#' \preformatted{
#' Start, 2006-04-08 12:00:00
#' Deadline, 2006-04-11 20:00:00
#' Key, Description,            Effort
#'   1, Code read.burndown(),    4
#'   2, Code summary.burndown(), 1
#'   3, Code plot.burndown(),    5
#'   4, Create R package,        2
#'   5, Write documentation,     2
#'   6, Set up website,          1
#' Key, Done, Time
#'   1,    5, 2006-04-08 13:00:00
#'   2,    5, 2006-04-08 13:30:00
#'   1,   10, 2006-04-08 14:00:00
#'   2,   50, 2006-04-08 15:00:00
#'   4,    5, 2006-04-08 19:30:00
#'   5,    5, 2006-04-08 20:00:00
#'   4,  100, 2006-04-08 21:16:00
#'   1,   50, 2006-04-09 09:10:00
#'   3,    5, 2006-04-09 09:41:00
#'   3,   30, 2006-04-09 10:18:00
#'   3,   80, 2006-04-09 11:00:00
#'   2,   60, 2006-04-09 12:00:00
#'   2,  100, 2006-04-09 12:10:00
#'   1,   70, 2006-04-09 12:30:00
#'   5,   30, 2006-04-09 13:50:00
#'   5,   90, 2006-04-09 14:20:00
#'   5,  100, 2006-04-09 14:30:00
#'   1,  100, 2006-04-09 14:35:00
#'   3,  100, 2006-04-09 14:40:00
#'   6,  100, 2006-04-09 16:00:00
#' }
#'
#' @param file a connection or a character string giving the name of the file
#' to load.
#' @param debug boolean, set to `TRUE` to print debugging information.
#' @return A burndown object.
#' @author Dan Kelley
#' @family things related to burndown data
#' @examples
#' library(plan)
#' filename <- system.file("extdata", "burndown.dat", package="plan")
#' b <- read.burndown(filename)
#' summary(b)
#' plot(b)
#' @export
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
    # Start, ISdate
    tokens <- trimws(scan(file, what='char', sep=",", nlines=1,quiet=quiet,blank.lines.skip=TRUE))
    name <- tokens[1]
    if (name != "Start") stop("First line of file must be 'Start' followed by an ISO date but got '",
        paste(tokens, collapse=","))
    start <- as.POSIXct(tokens[2])
    # Deadline, ISOdate
    tokens <- trimws(scan(file,what='char',sep=",",nlines=1,quiet=quiet,blank.lines.skip=TRUE))
    name <- tokens[1]
    deadline <- as.POSIXct(tokens[2])
    if (name != "Deadline") stop("Second line of file must be 'Deadline' followed by an ISO date, but got '",
        paste(tokens, collapse=","), "'")
    # Header
    tokens <- trimws(scan(file,what='char',sep=',',nlines=1,quiet=quiet,blank.lines.skip=TRUE))
    check.tokens(tokens, c("Key", "Description", "Effort"))
    task.key <- c()
    task.description <- c()
    task.effort <- c()
    while (TRUE) { # TASK: key description effort
        tokens <- trimws(scan(file, what=character(0),nlines=1,blank.lines.skip=FALSE,quiet=quiet,sep=","))
        if (tokens[1] == "Key")
            break
        if (3 == length(tokens)) {
            task.key <- c(task.key, as.numeric(tokens[1]))
            task.description <- c(task.description, tokens[2])
            task.effort <- c(task.effort, as.numeric(tokens[3]))
        }
    }
    # "Key,	Progress, Time", followed by data lines
    check.tokens(tokens, c("Key", "Done", "Time"))
    progress.key <- progress.done <- progress.time <- NULL
    while (TRUE) {
        tokens <- trimws(scan(file, what=character(0),nlines=1,blank.lines.skip=FALSE,quiet=quiet, sep=","))
        if (is.na(tokens[1]))
            break
        key <- as.numeric(tokens[1])
        if (!(key %in% task.key)) {
            msg <- paste("Progress key",key,"not in the list of task keys\n\tOffending line in data file follows\n\t",tokens[1]," ",tokens[2], " ", tokens[3])
            stop(msg)
        }
        done <- as.numeric(tokens[2])
        time <- as.POSIXct(tokens[3])
        progress.key <- c(progress.key, key)
        progress.done <- c(progress.done, done)
        progress.time <- c(progress.time, time)
    }
    # class(progress.time) <- "POSIXct"
    progress.time <- as.POSIXct(progress.time, origin=as.POSIXct("1970-01-01 00:00.00", tz="UTC"))
    # BUG: should ensure item is in task
    o <- order(progress.time)
    progress.key <- progress.key[o]
    progress.done <- progress.done[o]
    progress.time <- progress.time[o]
    rval <- new("burndown")
    rval@data <- list(start=start,
        deadline=deadline,
        tasks=list(key=task.key,
            description=task.description,
            effort=task.effort),
        progress=list(key=progress.key,
            progress=progress.done,
            time=progress.time))
    rval
}


#' Summarize a burndown object
#'
#' Print a summary of a burndown dataset.
#'
#' @param object A [burndown-class] object.
#'
#' @param ... ignored.
#'
#' @author Dan Kelley
#'
#' @family things related to burndown data
#'
#' @examples
#' library(plan)
#' data(burndown)
#' summary(burndown)
setMethod(f="summary",
    signature="burndown",
    definition=function(object, ...) {
        cat(paste("Start,   ", format(object[["start"]])), "\n")
        cat(paste("Deadline,", format(object[["deadline"]])), "\n")
        num.tasks <- length(object[["tasks"]]$key)
        dspace <- max(nchar(object[["tasks"]]$description))
        cat(sprintf("Key, Description,%s %5s\n",
                paste(rep(" ", dspace - nchar("Description")), collapse=""),
                "Effort"))
        for (i in 1:num.tasks) {
            space <- paste(rep(" ", dspace - nchar(object[["tasks"]]$description[i])), collapse="")
            cat(sprintf("%3s, %s,%s %s\n",
                    object[["tasks"]]$key[i], object[["tasks"]]$description[i], space, object[["tasks"]]$effort[i]))
        }
        cat("Key, Done,  Time\n")
        num.progress <- length(object[["progress"]]$key)
        for (i in 1:num.progress) {
            cat(sprintf("%3s, %5s, ", object[["progress"]]$key[i], object[["progress"]]$progress[i]))
            cat(format((object[["progress"]]$time[i])))
            cat("\n")
        }
        invisible()
    })

#' Create a burndown object
#'
#' Create a [burndown-class] object from the given data.
#'
#' Creates a [burndown-class] object from the given data; progress may
#' be given in percentage or absolute values.
#'
#' @param start Start date
#'
#' @param deadline Deadline (end date)
#'
#' @param tasks Data frame containing the task IDs (may be alphanumeric),
#' their description and effort
#'
#' @param progress Data frame containing the progress values with task ID, timestamp and work done (either in percentage or absolute)
#'
#' @param progressInPercent boolean; if set to `FALSE`, progress values are treated like absolute values and
#' converted to percentages
#'
#' @return A burndown object.
#'
#' @author Frank Schmitt
#'
#' @family things related to burndown data
#'
#' @examples
#' library(plan)
#' # same data as in tests/burndown.dat
#' start <- as.POSIXct(strptime("2006-04-08 12:00:00", "%Y-%m-%d %H:%M:%S"))
#' deadline <- as.POSIXct(strptime("2006-04-11 20:00:00", "%Y-%m-%d %H:%M:%S"))
#' tasks <- data.frame(key = c(1, 2, 3, 4, 5, 6),
#'     description = c("code read.burndown()", "code summary.burndown()", 
#'         "code plot.burndown()", "create R package", 
#'         "write documentation", "set up website"),
#'     effort = c(4, 1, 5, 2, 2, 1),
#'     stringsAsFactors = FALSE)
#' progress <- data.frame(key = c(1, 2, 1, 2, 4, 5, 4, 1, 3, 3, 3, 2, 2, 1, 5, 5, 5, 1, 3, 6),
#'     progress = c(5, 5, 10, 50, 5, 5, 100, 50, 5, 30, 80, 60, 
#'         100, 70, 30, 90, 100, 100, 100, 100),
#'     time = structure(c(1144494000, 1144495800, 1144497600, 1144501200, 
#'             1144517400, 1144519200, 1144523760, 1144566600, 
#'             1144568460, 1144570680, 1144573200, 1144576800, 
#'             1144577400, 1144578600, 1144583400, 1144585200,
#'             1144585800, 1144586100, 1144586400, 1144591200), 
#'         class = "POSIXct"),
#'     stringsAsFactors = FALSE
#' )
#' b <- as.burndown(start, deadline, tasks, progress, progressInPercent = TRUE)
#' summary(b)
#' plot(b)
#' @export
as.burndown <- function(start, deadline, tasks, progress, progressInPercent=FALSE)
{
    progress_percentage <- progress
    # if progress was given in absolute values: calculate percentage
    if (!progressInPercent) {
        progress_percentage$progress <- mapply(
            function(itskey, itsprogress) {
                itsprogress / subset(tasks, get("key", tasks) == itskey)$effort * 100
            },
            progress$key,
            progress$progress
        )
    }
    rval <- new("burndown")
    rval@data <- list(
        start = start,
        deadline = deadline,
        tasks = tasks,
        progress = progress_percentage)
    rval
}
