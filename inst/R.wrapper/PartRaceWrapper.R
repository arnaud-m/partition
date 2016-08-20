###############################################
## Racing methods for the selection of the best

race.init<-function(tasks, candidates, timeout = NA) {
  ## Load libraries and build the data list
  ## Args
  ## candidate : named list of candidate functions
  ## task      : list of tasks/instances.
  ## timeout   : timeout of each run
  stopifnot(
    is.list(tasks),
    length(tasks) > 1,
    is.character(candidates),
    length(candidates) > 1
    )
  ## Load function : eval with timeout.
  if( !is.numeric(timeout) || length(timeout) != 1 ||
     timeout <= 0 || !requireNamespace('R.utils') ) {
    ## cancel timeout
    timeout <- NA
  }
  ## Return init list
  return(list(no.candidates = length(candidates),
              no.tasks = length(tasks),
              tasks = tasks,
              candidates = candidates,
              timeout = timeout
              ))
}


race.info<-function(data) {
  ## Build info list
  ## Args:
  ## data : named list of candidates and tasks 
  timeout.msg <- ifelse(is.na(data$timeout),
                        "No timeout.",
                        paste("Each run has a timeout of", data$timeout,"seconds.")          
                        )
  return(
    list(race.name="Partition problem",
         no.candidates=(data$no.candidates),
         no.tasks=data$no.tasks,
         extra=paste(
           "We select the best algorithm for the partition problem.",
           "The partition problem consists in finding a bi-partition of a set of integers which minimizes the difference between their sums.",
           timeout.msg
           ))
    )
}


race.wrapper<-function(candidate, task, data){
  ## A candidate solves the partition problem associated to the task with or without timeout.
  ## Args
  ## candidate : index of the candidate
  ## task      : index of the task
  ## data      : named list of candidates and tasks 
  stopifnot(
    candidate >= 1,
    candidate <= data$no.candidates,
    task >= 1,
    task <= data$no.tasks
    )
  ## Retrieve the task to perform 
  sizes <- data$tasks[[task]]
  
  ## Eval the candidate on the task
  callCand <- call(data$candidates[candidate], sizes)
  if(is.na(data$timeout)) {
    ## No timeout
    part <- eval(callCand)
  } else {
    ## With timeout
      part <- tryCatch(
                R.utils::evalWithTimeout(eval(callCand), timeout = data$timeout),
                  error = function(ex) {
          warning(paste("[TIMEOUT]", race.describe(candidate, data), ": task", task),call.=FALSE)
          return(logical(length(sizes)))
        }
    )

  }
  ## Check for invalid partition. 
  if(!is.logical(part) || length(part) != length(sizes) ) {
    warning(paste("[FAIL] Function ", race.describe(candidate, data), ": task", task), call.=FALSE)
    ## Replaced by an empty partition
    part <- logical(length(sizes))
  }
  ## Return objective value
  return(PartEval(sizes,part))
}

race.describe<-function(candidate,data) {
  ## Describe the candidate
  ## Args:
  ## candidate : index of the candidate
  stopifnot(
    is.list(data),
    candidate >= 1,
    candidate <= data$no.candidates
    )
  return(data$candidates[candidate])
}
