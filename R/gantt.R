#' Class to store gantt objects
#'
#' These objects may be created with [as.gantt()] or [read.gantt()].
#' @family things related to gantt data
#' @importFrom methods new
setClass("gantt", contains="plan")

setMethod(f="initialize",
    signature="gantt",
    definition=function(.Object) {
        .Object@data <- list(description=NULL, start=NULL, end=NULL, done=NULL, neededBy=NULL, key=NULL)
        return(.Object)
    })


#' Draw a Gantt diagram
#'
#' Plot a gantt chart that shows the time allocated to a set of tasks, optionally
#' also with an indication of discrete events that occur as instants in time.
#'
#' Time is indicated along the x axis, and tasks are stacked along the y
#' axis, akin to progress bars. Colour-coding can be used to indicate the degree of
#' completion of each task. These codes can be set individually for individual
#' tasks. Progress bars can have arrows (on either end), suggesting tasks
#' with flexible start/end dates or overdue tasks.  Vertical lines may
#' be drawn for discreet events. See \dQuote{Examples} for a few of the
#' possibilities.
#'
#' @param x A [gantt-class] object.
#' @param xlim optional range of time axis; if not provided, the range of times
#' in `x` will be used.
#' @param time.format format for dates on time axis; defaults to 3-letter
#' month.
#' @param time.labels.by suggested label increment on time axis, e.g.
#' `time.labels.by="2 months"` to get a two-month interval.  If not
#' supplied, the axis will be generated automatically.
#' @param time.lines.by suggested interval between vertical grid lines on the
#' plot, e.g. `time.lines.by="1 week"` for weekly.  If not supplied, the
#' grid will be generated automatically.
#' @param event.time vector of event times, e.g. conferences, whose time cannot
#' be altered.
#' @param event.label vector of character strings holding event names.
#' @param event.side side for event labels.
#' @param col.connector colour of (optional) connectors between items.
#' @param col.done colour of work that has been done already. This may be a
#' vector of colours, one for each item in the gantt table.
#' @param col.notdone colour of work that has not been done yet. This may be a
#' vector of colours, one for each item in the gantt table.
#' @param col.eventLine colour of event lines; may be a vector.
#' @param col.event colour of event labels; may be a vector.
#' @param cex.event expansion factor for event labels; may be a vector.
#' @param lty.eventLine line type for event lines; may be a vector.
#' @param lwd.eventLine line width for event lines; may be a vector.
#' @param font.event font for event labels; may be a vector.
#' @param bg background colour for plot.
#' @param grid.col colour for grid.
#' @param grid.lty line type for grid.
#' @param ylabels A [list] with elements `col` for colour,
#' `cex` for character-expansion factor, `font` for font, and `justification`
#' for the placement in the margin (`0` means left-justified, and `1`
#' means right-justified. (NOTE: left-justification works poorly in RStudio, but
#' properly in other systems.)
#' It usually makes sense for the elements in `ylabels` to be vectors of the same
#' length as the topic list. However, shorter vectors are permitted, and they lengthened by
#' copying the default values at the end (see Example 6).
#' @param arrows A vector of strings, one for each topic, indicating the nature of
#' the arrows that may be drawn at the ends of task bars. The individual values
#' may be `"left"`, `"right"`, `"both"` or `"neither"`.
#' Set `arrows=NULL`, the default, to avoid such arrows.
#' @param main character string to be used as chart title.
#' @param line.main line where title occurs. If `NA`, then the
#' title is placed in a default location; otherwise, it is `line.main`
#' lines above the top of the plot.
#' @param cex.main numeric, font-size factor for title.
#' @param mgp setting for [par]`(mgp)`, within-axis spacing. The
#' default value tightens axis spacing.
#' @param maiAdd inches to add to the auto-computed margins at the bottom,
#' left, top, and right margins. The values may be negative (to tighten
#' margins) but the sum will be truncated to remain positive.
#' @param axes logical, `TRUE` to draw the x axis. (Setting to
#' `FALSE` permits detailed axis tweaking.)
#' @param debug logical value, `TRUE` to monitor the work.
#' @param ... extra things handed down.
#' @author Dan Kelley
#' @family things related to gantt data
#' @references Gantt diagrams are described on wikipedia
#' `https://en.wikipedia.org/wiki/Gantt_Chart`.
#'
#' @examples
#' library(plan)
#' data(gantt)
#' summary(gantt)
#'
#' # 1. Simple plot
#' plot(gantt)
#'
#' # 2. Plot with two events
#' event.label <- c("Proposal", "AGU")
#' event.time <- c("2008-01-28", "2008-12-10")
#' plot(gantt, event.label=event.label,event.time=event.time)
#'
#' # 3. Control x axis (months, say)
#' plot(gantt,labels=paste("M",1:6,sep=""))
#'
#' # 4. Control task colours
#' plot(gantt,
#'      col.done=c("black", "red", rep("black", 10)),
#'      col.notdone=c("lightgray", "pink", rep("lightgray", 10)))
#'
#' # 5. Control event colours (garish, to illustrate)
#' plot(gantt, event.time=event.time, event.label=event.label,
#'      lwd.eventLine=1:2, lty.eventLine=1:2,
#'      col.eventLine=c("pink", "lightblue"),
#'      col.event=c("red", "blue"), font.event=1:2, cex.event=1:2)
#'
#' # 6. Top task is in bold font and red colour
#' plot(gantt,ylabels=list(col="red",font=2))
#'
#' # 7. Demonstrate zero-time item (which becomes a heading)
#' gantt[["description"]][1] <- "Preliminaries"
#' gantt[["end"]][1] <- gantt[["start"]][1]
#' plot(gantt, ylabel=list(font=2, justification=0))
#'
#' # 8. Arrows at task ends
#' plot(gantt, arrows=c("right","left","left","right"))
#' @aliases plot.gantt
#' @importFrom grDevices gray hcl
#' @importFrom graphics abline axis axis.POSIXct box grconvertX legend lines mtext par points polygon rect rug strheight strwidth text
#' @export
setMethod(f="plot",
    signature=signature("gantt"),
    definition=function (x, xlim,
        time.format=NULL, time.labels.by, time.lines.by,
        event.time=NULL, event.label=NULL, event.side=3,
        col.connector="black",
        col.done=gray(0.3), col.notdone=gray(0.9),
        col.eventLine=gray(0.1), col.event=par("fg"),
        cex.event=par("cex"), font.event=par("font"),
        lty.eventLine=par("lty"), lwd.eventLine=par("lwd"),
        bg=par("bg"), grid.col="lightgray", grid.lty="dotted",
        ylabels=list(col=1, cex=1, font=1, justification=1),
        arrows=NULL,
        main="", line.main=NA, cex.main=par("cex"),
        mgp=c(2, 0.7, 0), maiAdd=rep(0, 4),
        axes=TRUE,
        debug=FALSE, ...)
{
    if (!inherits(x, "gantt")) stop("method is only for gantt objects")
    opar <- par(no.readonly = TRUE)
    half.height <- 0.33
    t0 <- as.POSIXct("1970-01-01 00:00:00")
    ## Lengthen anything that can be a vector
    ndescriptions <- length(x[["description"]])
    if (length(arrows) == 0L)
        arrows <- rep("none", ndescriptions)
    if (length(arrows) < ndescriptions)
        arrows <- c(arrows, rep("none", ndescriptions-length(arrows)))
    ## Twiddle the labels, including defaulting things that a user
    ## need not define.
    if (!("col" %in% names(ylabels)))
        ylabels$col <- 1
    if (!("cex" %in% names(ylabels)))
        ylabels$cex <- 1
    if (!("font" %in% names(ylabels)))
        ylabels$font <- 1
    if (!("justification" %in% names(ylabels)))
        ylabels$justification <- 1
    for (i in seq_along(ylabels)) {
        len <- length(ylabels[[i]])
        if (len < ndescriptions) {
            ylabels[[i]] <- c(ylabels[[i]], rep(1, ndescriptions-len))
        }
    }
    if (any(!(ylabels$justification %in% c(0, 1))))
        stop("ylabels$justification entries must be 0 or 1")
    if (length(col.done) < ndescriptions)
        col.done <- rep(col.done, length.out=ndescriptions)
    if (length(col.notdone) < ndescriptions)
        col.notdone <- rep(col.notdone, length.out=ndescriptions)
    nevent <- length(event.time)
    if (length(col.eventLine) < nevent)
        col.eventLine <- rep(col.eventLine, length.out=nevent)
    if (length(col.event) < nevent)
        col.event <- rep(col.event, length.out=nevent)
    if (length(cex.event) < nevent)
        cex.event <- rep(cex.event, length.out=nevent)
    if (length(font.event) < nevent)
        font.event <- rep(font.event, length.out=nevent)
    if (length(lty.eventLine) < nevent)
        lty.eventLine <- rep(lty.eventLine, length.out=nevent)
    if (length(lwd.eventLine) < nevent)
        lwd.eventLine <- rep(lwd.eventLine, length.out=nevent)

    charheight <- strheight("M", units = "inches")
    maxwidth <- max(strwidth(x[["description"]], units = "inches")) * 1.1

    ## Get around some problems with autoscaling of POSIXt values
    r <- if (missing(xlim)) range(x[["start"]], x[["end"]], na.rm=TRUE) else xlim
    if (debug) {cat("range: ", as.character(r[1]), "to", as.character(r[2]), "\n")}
    s <- as.numeric(difftime(r[2], r[1], units="days"))
    r <- as.POSIXlt(r)
    subTics <- NULL
    if (s > 100) {
        if (is.null(time.format)) time.format <-  "%b %Y" # month/year
        r[2] <- r[2] + 86400
        r[1:2]$hour <- r[1:2]$min <- r[1:2]$sec <- 0
        if (debug){cat("range: ", as.character(r[1]), "to", as.character(r[2]), "\n")}
        ## monthly ticks
        lhs <- as.POSIXlt(r[1])
        lhs$mon <- 0
        lhs$mday <- 1
        rhs <- as.POSIXlt(r[2])
        rhs$mon <- 11
        rhs$mday <- 31
        subTics <- seq(lhs, rhs, by="month")
    } else {
        if (s > 10) {
            if (is.null(time.format)) time.format <-  "%d/%b" # day/month
            r[2] <- r[2] + 86400
            r[1:2]$hour <- r[1:2]$min <- r[1:2]$sec <- 0
            if(debug){cat("range: ", as.character(r[1]), "to", as.character(r[2]), "\n")}
        } else {
            if (s > 1) {
                if (is.null(time.format)) time.format <-  "%d/%b" # day/month
                r[2] <- r[2] + 86400
                r[1:2]$hour <- r[1:2]$min <- r[1:2]$sec <- 0
                if(debug){cat("range: ", as.character(r[1]), "to", as.character(r[2]), "\n")}
            } else {
                if (is.null(time.format)) time.format <-  "%d/%b" # day/month
            }
        }
    }
    bottom.margin <- 0.5
    if (is.na(line.main))
        line.main <- if (nevent==0) 0.5 else 0.5 + cex.event[1]
    topSpace <- charheight * (2 + line.main)
    mai <- maiAdd + c(bottom.margin, maxwidth, topSpace, 0.25)
    mai <- ifelse(mai < 0, 0, mai)
    opar <- par(no.readonly = TRUE)
    par(mgp=mgp, mai=mai, omi=c(0.1, 0.1, 0.1, 0.1), bg=bg)
    plot(c(r[1], r[2]), c(1,2*ndescriptions),
         ylim=c(0.5, ndescriptions + 0.5),
         xaxs="i", yaxs="i",
         bg=bg,
         main="", xlab="", ylab="", xaxs="r", type="n", axes=FALSE)
    xlim <- as.POSIXct(par("usr")[1:2] + t0)
    box()
    if (nchar(main)) {
        mtext(main, side=3, line=line.main, cex=cex.main)
    }
    if (axes) {
        if (missing(time.labels.by)) {
            ##xaxp <- par("xaxp")
            lines.at.0 <- axis.POSIXct(1,
                at=pretty(r, 10), #seq(xaxp[1], xaxp[2], length.out=xaxp[3]) + t0,
                format=time.format, cex.axis=par("cex.axis"), ...)
        } else {
            lines.at.0 <- axis.POSIXct(1,
                at=as.POSIXct(seq.POSIXt(as.POSIXct(xlim[1]), as.POSIXct(xlim[2]), by=time.labels.by)),
                format=time.format, cex.axis=par("cex.axis"), ...)
        }
    }
    if (axes) {
        if (!is.null(subTics))
            rug(subTics, quiet=TRUE)
        if (missing(time.lines.by)) {
            abline(v=lines.at.0, col = grid.col, lty=grid.lty)
        } else {
            abline(v = seq.POSIXt(as.POSIXct(xlim[1]), as.POSIXct(xlim[2]), by=time.lines.by), col = grid.col, lty=grid.lty)
        }
    }
    topdown <- seq(ndescriptions, 1)
    font <- rep(1, ndescriptions)
    font[2] <- 2
    axis(2, at=topdown, labels=rep("", ndescriptions), las=2, tick=FALSE, cex.axis=par("cex.axis"))
    par(xpd=NA)
    for (i in 1:ndescriptions) {
        if (ylabels$justification[i] == 1) {
            left <- par('usr')[1]
            text(left, topdown[i], x[["description"]][i], pos=2,
                 col=ylabels$col[i], cex=ylabels$cex[i], font=ylabels$font[i])
        } else {
            left <- grconvertX(0, 'device', 'user')
            ## warning("In plot() method for gantt objects :\n  justification=0 places labels poorly in RStudio, better in other systems",
            ##         call.=FALSE)
            ## message("  left= ", left, " (the thick black line is there)")
            ## message("  Q: why is this black line not at the left of the graph?")
            text(left, topdown[i], x[["description"]][i], pos=4,
                 col=ylabels$col[i], cex=ylabels$cex[i], font=ylabels$font[i])
            ## abline(v=left, lwd=10, col='red')
        }
    }
    par(xpd=FALSE)

    ## Connectors
    for (t in 1:ndescriptions) {
        nb <- x[["neededBy"]][t][[1]]
        if (!is.na(nb)) {
            source.y <- topdown[t]
            source.t <- as.POSIXct(x[["end"]][t])
            for (nbi in 1:length(nb)) {
                r <- as.numeric(nb[nbi])
                receiver.t <- as.POSIXct(x[["start"]][r])
                receiver.y <- topdown[r]
                lines(c(source.t,receiver.t), c(source.y,receiver.y),col=col.connector)
            }
        }
    }
    ## Events
    if (!is.null(event.time)) {
        ne <- length(event.time)
        for (e in 1:ne) {
            t <- as.POSIXct(event.time[e])
            abline(v=t, col=col.event[e], lwd=lwd.eventLine[e], lty=lty.eventLine[e])
            mtext(event.label[e], side=event.side, at=t,
                  col=col.event[e], font=font.event[e], cex=cex.event[e])
        }
    }
    ## Description
    for (i in 1:ndescriptions) {
        if (!is.na(x[["start"]][i])) {
            mid <- as.POSIXct(x[["start"]][i]) +
            x[["done"]][i] * as.numeric(difftime(as.POSIXct(x[["end"]][i]),
                                                 as.POSIXct(x[["start"]][i]),
                                                 units="secs")) / 100
            if (debug) {cat(as.character(x[["description"]][i]),"takes", as.numeric(difftime(as.POSIXct(x[["end"]][i]), as.POSIXct(x[["start"]][i]), units="secs")), "s\n")}

            bottom <- topdown[i] - half.height
            top <- topdown[i] + half.height
            left <- as.POSIXct(x[["start"]][i])
            right <- as.POSIXct(x[["end"]][i])

            if (debug){cat(as.character(x[["description"]][i]));cat(" done=",x[["done"]][i]," mid=");print(mid);cat(" left=");print(left);cat("right=");print(right);cat("\n")}

            if (right > left) {
                arrow <- arrows[i]
                rect(left, bottom, right, top, col = col.notdone[i], border = FALSE)
                rect(left, bottom, mid,   top, col = col.done[i],    border = FALSE)
                rect(left, bottom, right, top, col = "transparent",  border = TRUE)
                usr <- par('usr')
                D <- (top - bottom) * (usr[2]-usr[1]) / (usr[4]-usr[3])
                D <- 0.02 * (usr[2] - usr[1])
                if (arrow == "left" || arrow == "both") {
                    colTriangle <- if (left == mid) col.notdone else col.done
                    polygon(c(left, left-D, left), c(bottom, 0.5*(bottom+top), top),
                        border=colTriangle[i], col=colTriangle[i])
                    lines(c(left, left-D, left), c(bottom, 0.5*(bottom+top), top))
                }
                if (arrow == "right" || arrow == "both") {
                    colTriangle <- if (right == mid) col.done else col.notdone
                    polygon(c(right, right+D, right), c(bottom, 0.5*(bottom+top), top),
                        border=colTriangle[i], col=colTriangle[i])
                    lines(c(right, right+D, right), c(bottom, 0.5*(bottom+top), top))
                }
            }
        }
    }
    abline(h = (topdown[1:(ndescriptions - 1)] + topdown[2:ndescriptions])/2,  col = grid.col, lty=grid.lty)
    par(opar)
    invisible(x)
})


#' Summarize a gantt object
#'
#' Summarizes a gantt object.
#'
#' Prints a summary of a gantt dataset.
#'
#' @param object A [gantt-class] object.
#' @param ... ignored.
#' @author Dan Kelley
#' @family things related to gantt data
#' @references
#' \url{https://en.wikipedia.org/wiki/Burndown_chart}
#' @examples
#' library(plan)
#' data(gantt)
#' summary(gantt)
#' @export
setMethod(f="summary",
    signature="gantt",
    definition=function(object, ...) {
        if (length(object@data[[1]])) {
            max.description.width <- max(nchar(as.character(object[["description"]])))
            num.descriptions <- length(object[["description"]])
            cat("Key, Description,", paste(rep(" ", max.description.width-12), collapse=""), "Start,      End,        Done, NeededBy\n")
            for (t in 1:num.descriptions) {
                spacer <- paste(rep(" ", 1 + max.description.width - nchar(as.character(object[["description"]][t]))),
                    collapse="")
                cat(paste(format(object[["key"]][t], width=3, justify="right"), ",", sep=""),
                    paste(as.character(object[["description"]][t]), ",",
                        spacer,
                        format(object[["start"]][t]), ", ",
                        object[["end"]][t],  ", ",
                        format(object[["done"]][t], width=4, justify="right"), sep = ""))
                nb <- object[["neededBy"]][t][[1]]
                if (!is.null(nb) && !is.na(nb[1])) {
                    cat(", ")
                    for (nbi in 1:length(nb)) {
                        cat(object[["description"]][as.numeric(nb[nbi])], " ")
                    }
                }
                cat("\n")
            }
        } else {
            cat("empty\n")
        }
    })


#' Create a gantt object.
#'
#' This creates a [gantt-class] object.
#'
#' @param key integer key for task, normally 1 for the first task, 2 for the
#' second, etc.
#' @param description character string describing the task (brief)
#' @param start start date for task (POSIXt or character string that converts
#' to POSIXt with [as.POSIXct()]
#' @param end end date for task (POSIXt or character string that converts to
#' POSIXt with [as.POSIXct()].
#' @param done percentage completion for the task
#' @param neededBy optional key for a dependent task
#' @return A [gantt-class] object; for details, see [read.gantt()].
#' @author Dan Kelley
#' @family things related to gantt data
#' @examples
#'
#' library(plan)
#' arrive <- as.POSIXct("2012-09-05")
#' month <- 28 * 86400
#' year <- 12 * month
#' leave <- arrive + 4 * year
#' startT1 <- arrive
#' endT1 <- startT1 + 4 * month
#' startT2 <- endT1 + 1
#' endT2 <- startT2 + 4 * month
#' startQE <- arrive + 9 * month
#' endQE <- arrive + 12 * month
#' QEabsoluteEnd <- arrive + 15 * month
#' startProposal <- arrive + 15 * month # for example
#' endProposal <- arrive + 20 * month
#' startThesisWork <- arrive + 2 * month # assumes no thesis work until 2 months in
#' endThesisWork <- leave - 4 * month
#' startWriting <- leave - 36 * month
#' endWriting <- leave
#' g <- as.gantt(key=1:8, c("Academic",
#'               "Term 1 classes",
#'               "Term 2 classes",
#'               "Qualifying Examination",
#'               "Research",
#'               "Proposal Defence",
#'               "Thesis Work",
#'               "Paper/Thesis Writing"),
#'               c(startT1, startT1, startT2, startQE, startProposal, startProposal,
#'                 startThesisWork, startWriting),
#'               c(startT1, endT1, endT2, endQE, startProposal, endProposal,
#'                 endThesisWork, endWriting),
#'               done=rep(0, 7))
#' plot(g, xlim=c(arrive, leave),
#'      ylabel=list(font=c(2,rep(1,3),2), justification=c(0,rep(1,3),0)))
#' @export
as.gantt <- function(key, description, start, end, done, neededBy)
{
    if (missing(key))
        stop("must give 'key'")
    if (missing(description))
        stop("must give 'description'")
    if (missing(start))
        stop("must give 'start'")
    if (missing(end))
        stop("must give 'end'")
    n <- length(key)
    if (missing(done))
        done <- rep(0, n)
    if (missing(neededBy))
        neededBy <- rep(NA, n)
    rval <- new("gantt")
    rval@data <- list(key=key,
        description=as.character(description),
        start=as.POSIXct(start),
        end=as.POSIXct(end),
        done=done,
        neededBy=neededBy)
    rval
}





#' Read a gantt data file
#'
#' Read a data file containing gantt information.
#' The data format is strict, and deviations from it may lead to error messages
#' that are difficult to understand; see \dQuote{Details}.
#'
#' The first line is a header, and must contain the words `Key`,
#' `Description`, `Start`, `End`, `Done`, and
#' `NeededBy`, written exactly in this way, with commas separating the
#' words.  (Blanks are ignored in this line.)
#'
#' Additional lines indicate the details of each of several sub-projects, in
#' comma-separated items, as follows:
#'
#' * A key for the task.  These must be distinct, and are
#' typically just the numbers 1, 2, 3, etc.
#'
#' * A description of the task.  (This may not contain commas!)
#'
#' * The start time for the task, in ISO 8601 format (`YYYY-MM-DD` or
#' `YYYY-MM-DD hh:mm:ss`).
#'
#' * The end time for the task, in the same format as the starting time. If
#' an end time equals the corresponding start time, no rectangle will be drawn
#' for the activity, and this gives a way to make headings (see example 7
#' for [plot,gantt-method()]).
#'
#' * A number indicating the percentage of this task that has been
#' completed to date.
#'
#' * A space-separated optional list of numbers that indicate the keys of
#' other tasks that depend on this one.  This list is ignored in the present
#' version of [read.gantt()].
#'
#' @section Sample data file:
#'```
#' Key, Description,                 Start,        End, Done, NeededBy
#'   1, Assemble equipment,     2008-01-01, 2008-03-28, 90
#'   2, Test methods,           2008-02-28, 2008-03-28, 30
#'   3, Field sampling,         2008-04-01, 2008-08-14, 0
#'   4, Analyse field data,     2008-06-30, 2008-11-14, 0
#'   5, Write methods chapter,  2008-08-14, 2008-11-14, 0
#'   6, Write results chapter,  2008-10-14, 2009-01-15, 0
#'   7, Write other chapters,   2008-12-10, 2009-02-28, 0
#'   8, Committee reads thesis, 2009-02-28, 2009-03-14, 0
#'   9, Revise thesis,          2009-03-15, 2009-03-30, 0
#'  10, Thesis on display,      2009-04-01, 2009-04-15, 0
#'  11, Defend thesis,          2009-04-16, 2009-04-17, 0
#'  12, Finalize thesis,        2009-04-18, 2009-05-07, 0
#'```
#'
#' @param file a connection or a character string giving the name of the file
#' to load.
#' @param debug boolean, set to `TRUE` to print debugging information.
#' @return A [gantt-class] object, which is a data frame containing
#' `description` (a character description of the task), `"start"`
#' (the task's start time), `"end"` (the task's end time),
#' `"progress"` (a number giving the percent progress on this item, or
#' `NA` if none given), and `needed.by` (a number giving the
#' indices of other tasks that rely on this task, or `NA` if none given).
#' @author Dan Kelley
#' @family things related to gantt data
#' @examples
#' library(plan)
#' filename <- system.file("extdata", "gantt.dat", package="plan")
#' g <- read.gantt(filename)
#' summary(g)
#' plot(g)
#'
#' @export
read.gantt <- function(file, debug=FALSE)
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
    tokens <- trimws(scan(file,what='char',sep=",",nlines=1,quiet=quiet))
    check.tokens(tokens, c("Key", "Description", "Start", "End", "Done", "NeededBy"))
    key <- description <- start <- end <- done <- neededBy <- c()
    while (TRUE) {
        tokens <- trimws(scan(file, what=character(0), nlines=1,
                blank.lines.skip=FALSE, quiet=quiet, sep=","))
        ni <- length(tokens)
        if (ni > 1) {
            if (ni < 3) stop("need at least 3 items per line")
            key <- c(key, as.numeric(tokens[1]))
            description <- c(description, tokens[2])
            start <- c(start, tokens[3])
            end <- c(end, tokens[4])
            done <- c(done, if (ni >= 5) as.numeric(tokens[5]) else NA)
            neededBy <- c(neededBy, if (ni >= 6) as.numeric(tokens[6:ni]) else NA)
        } else {
            break
        }
    }
    as.gantt(key=key,
        description=as.character(description),
        start=as.POSIXct(start),
        end=as.POSIXct(end),
        done=done,
        neededBy=neededBy)
}

#' Add a task to a gantt object
#'
#' This can be a simpler method than using [as.gantt()], because
#' tasks can be added one at a time.
#'
#' @param g A [gantt-class] object.
#' @param description A character string describing the task.
#' @param start A character string indicating the task start time, in a format understood by [as.POSIXct()].
#' Set to `""` (the default) to indicate that `description` is a heading, with no start and end time.
#' @param end A character string indicating the end time, in a format understood by [as.POSIXct()].
#' @param done A numerical value indicating the fraction done.
#' @param neededBy An integer indicating a task that depends on the completion of this task. If this is
#' `NA`, then the task is not needed by any other task.
#' @param key An optional value indicating the desired key value. If not given, this will default to
#' one beyond the highest key in `g`. Otherwise, if `key` is an integer matching
#' a task that is already in `g`, then that task is replaced; otherwise, the new task
#' is placed between the tasks with integral keys on either side of the task. For example, setting
#' `key=4.5` places this between existing keys 4 and 5 (and then renumbers all keys
#' to be integers); see \dQuote{Examples}.
#'
#' @examples
#' library("plan")
#' g <- new("gantt")
#' g <- ganttAddTask(g, "Courses") # no times, so a heading
#' g <- ganttAddTask(g, "Physical Oceanography", "2016-09-03", "2016-12-05")
#' g <- ganttAddTask(g, "Chemistry Oceanography", "2016-09-03", "2016-12-05")
#' g <- ganttAddTask(g, "Fluid Dynamics", "2016-09-03", "2016-12-05")
#' g <- ganttAddTask(g, "Biological Oceanography", "2017-01-03", "2017-04-05")
#' g <- ganttAddTask(g, "Geological Oceanography", "2017-01-03", "2017-04-05")
#' g <- ganttAddTask(g, "Time-series Analysis", "2017-01-03", "2017-04-05")
#' g <- ganttAddTask(g, "Research") # no times, so a heading
#' g <- ganttAddTask(g, "Literature review", "2016-09-03", "2017-04-05")
#' g <- ganttAddTask(g, "Develop analysis skills", "2016-09-03", "2017-08-01")
#' g <- ganttAddTask(g, "Thesis work", "2017-01-01", "2018-04-01")
#' g <- ganttAddTask(g, "Defend thesis proposal", "2017-05-01", "2017-06-01")
#' g <- ganttAddTask(g, "Write papers & thesis", "2017-05-01", "2018-04-01")
#' g <- ganttAddTask(g, "Defend thesis", "2018-05-01", "2018-05-15")
#' # Set 'font' for bold-faced headings
#' font <- ifelse(is.na(g[["start"]]), 2, 1)
#' plot(g, ylabel=list(font=font))
#'
#' @family things related to gantt data
#' @export
ganttAddTask <- function(g, description="", start=NA, end=NA, done=0, neededBy=NA, key)
{
    if (!inherits(g, "gantt")) stop("method only applies to gantt objects")
    if (nchar(description) < 1) {
        warning("empty description")
    } else {
        nkey <- if (length(g[["key"]])) max(g[["key"]]) else 0
        if (missing(key))
            key <- 1 + nkey
        if (key < 1)
            stop("cannot have a key less than 1")
        if (key==as.integer(key)) {
            g[["description"]][key] <- description
            g[["start"]][key] <- start
            g[["end"]][key] <- end
            g[["done"]][key] <- done
            g[["neededBy"]][key] <- neededBy
            g[["key"]][key] <- key
        } else {
            before <- seq.int(1, floor(key))
            after <- seq.int(floor(key) + 1, nkey)
            message("nkey: ", nkey, ", key: ", key, ", before: ", paste(before, collapse=" "), ", after: ", paste(after, collapse=" "))
            g[["description"]] <- c(g[["description"]][before], description, g[["description"]][after])
            g[["start"]] <- c(g[["start"]][before], start, g[["start"]][after])
            g[["end"]] <- c(g[["end"]][before], end, g[["end"]][after])
            g[["done"]] <- c(g[["done"]][before], done, g[["done"]][after])
            g[["neededBy"]] <- c(g[["neededBy"]][before], neededBy, g[["neededBy"]][after])
            g[["key"]] <- c(g[["key"]][before], key, g[["key"]][after])
        }
    }
    g[["key"]] <- seq_along(g[["key"]])
    g
}
