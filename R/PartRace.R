#' Race to solve the partition problem
#'
#' Set up, execute, and log a race between several solvers (\code{candidates}) to solve partition problem's instances (\code{task}).
#' It requires the package \pkg{race}.
#' 
#' @param candidates a character vector which contains the names of the solver's functions
#' @param tasks a list of integer sets (numeric vector)
#' @inheritParams race::race
#' @param timeout per run in seconds (one candidate executes one task) that requires the package \pkg{R.utils} 
#' @param plot.file filename to plot the progress of the race.
#'
#' @seealso
#' \link[race]{race}, \link[race]{race.wrapper},
#'
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics axis barplot plot polygon
#' @export
#' @examples
#' ## Generate five random instances of the partition problem
#' tasks <- PartGenInt( m = 5)
#' ## Functions that return a random partition 
#' BeatMe <- function(sizes) sample(c(TRUE,FALSE),length(sizes), replace=TRUE)
#' BeatMeAgain <- BeatMe
#' ## vector of candidate names
#' candidates <- c("BeatMe", "BeatMeAgain")
#' ## Race between the two functions/algorithms
#' PartRace(candidates, tasks, plot.file = NA)
PartRace <- function(candidates, tasks, maxExp = 0,
             stat.test = c("friedman","t.bonferroni","t.holm","t.none"),
                     conf.level = 0.95, first.test = 10, interactive=TRUE, log.file="", 
                     timeout = NA, plot.file = "") {
                       
  loadNamespace('race')
  ## Check arguments
  stopifnot(
    is.character(candidates),
    sapply(candidates, exists, mode="function"),
    is.list(tasks),
    sapply(tasks, function(x) {is.numeric(x) && all(x > 0) })
    )
  ##TODO Check the size here
  
  ## Race
  race.data <-
    race::race(wrapper.file = system.file("R.wrapper", "PartRaceWrapper.R", package = "partition"),
         maxExp = maxExp,
         stat.test = stat.test,
         conf.level = conf.level,
         first.test = first.test,
         interactive = interactive,
         log.file = log.file,
         no.slaves = 0,
         ## Extra parameters for race.init
         tasks = tasks,
         candidates = candidates,
         timeout = timeout
         )
  
  ## Postprocess Race
  race.results <- race.data$results
  ## DEBUG
  ## race.results <- matrix(sample(c(NA,0), 50, replace=TRUE, prob=c(0.2,0.8)),ncol=5)
  
  mat <- rbind(is.na(race.results), !race.data$alive, TRUE)
  death <- apply(mat, 2, function(x) {which.max(x)})
  
  ## Final ranking
  ranking <- data.frame(name = candidates[order(death, decreasing=TRUE)],
                   rank = rank(sort(-death), ties.method = "max"))
  ## Display
  if(interactive) {
    cat("Ranking\n")
    print(ranking)
  }
  ## Plot
  if(is.character(plot.file) && nchar(plot.file) > 0) {
    cat(sprintf("Generate Race PDF Plot in %s..\n", plot.file))
    pdf(plot.file)
    
    y <- apply( !mat , 1, sum)
    x <- seq_along(y)
    ## Plot step function
    plot(y,  , type='s',
         xlab="Stage", yaxt="n",ylab="",
         cex=2)
    ## Fill step function
    polyx <- rep(x,each=2)
    polyy <- c(0, rep(utils::head(y,-1),each=2),0)
    polygon(polyx, polyy, col='lightgrey')
    ## Change y-axis
    axis(2, at=seq_along(candidates),
         labels=candidates,
         ##cex.axis=1.5,
         las=2)
    invisible(dev.off())
  }
  if(interactive) {
    cat("\nThat's all folks !\n")
  }
  return(invisible(list(ranking = ranking, race = race.data)))
}




