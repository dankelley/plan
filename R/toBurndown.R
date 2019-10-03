#' Create a \code{burndown} object
#'
#' Create a \code{burndown} object from the given data.
#'
#' Creates a \code{burndown} object from the given data; progress may be given in percentage or absolute values.
#'
#' @param start Start date
#' @param deadline Deadline (end date)
#' @param tasks Data frame containing the task IDs (may be alphanumeric), their description and effort
#' @param progress Data frame containing the progress values with task ID, timestamp and work done (either in percentage or absolute)
#' @param progress_in_percent boolean; if set to \code{FALSE}, progress values are treated like absolute values and
#' converted to percentages
#' @return A burndown object.
#' @author Frank Schmitt
#' @family things related to \code{burndown} data
#' @examples
#' \dontrun{
#' library(plan)
#' # same data as in tests/burndown.dat
#' start <- as.POSIXct(strptime("2006-04-08 12:00:00", "%Y-%m-%d %H:%M:%S"))
#' deadline <- as.POSIXct(strptime("2006-04-11 20:00:00", "%Y-%m-%d %H:%M:%S"))
#' tasks <- data.frame(key = c(1, 2, 3, 4, 5, 6),
#'                     description = c("code read.burndown()", "code summary.burndown()", 
#'                                     "code plot.burndown()", "create R package", 
#'                                     "write documentation", "set up website"),
#'                     effort = c(4, 1, 5, 2, 2, 1))
#' progress <- data.frame(key = c(1, 2, 1, 2, 4, 5, 4, 1, 3, 3, 3, 2, 2, 1, 5, 5, 5, 1, 3, 6),
#'                        progress = c(5, 5, 10, 50, 5, 5, 100, 50, 5, 30, 80, 60, 
#'                                     100, 70, 30, 90, 100, 100, 100, 100),
#'                        time = structure(c(1144494000, 1144495800, 1144497600, 1144501200, 
#'                                           1144517400, 1144519200, 1144523760, 1144566600, 
#'                                           1144568460, 1144570680, 1144573200, 1144576800, 
#'                                           1144577400, 1144578600, 1144583400, 1144585200,
#'                                           1144585800, 1144586100, 1144586400, 1144591200), 
#'                                           class = "POSIXct")
#'                        )
#' b <- toBurndown(start, deadline, tasks, progress, progress_in_percent = TRUE)
#' summary(b)
#' plot(b)
#' }
toBurndown = function(start,
                      deadline,
                      tasks,
                      progress,
                      progress_in_percent = FALSE) {
  progress_percentage = progress
  # if progress was given in absolute values: calculate percentage
  if (!progress_in_percent) {
    progress_percentage$progress = mapply(
      function(itskey, itsprogress) {
        itsprogress / subset(tasks, get("key") == itskey)$effort * 100
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
    progress = progress_percentage
  )
  rval
}