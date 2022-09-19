# Main function
# Author: nelliott
# Help from: lsalas

# Main function for evaluating restoration sites in CVJV
evaluate_restoration_site <- function(in_json) {
  
  log_file <- file.path(getwd(), "log_file.txt")
  
  # Start log
  tryCatch({
    
    log_conn <- file(log_file, "w")
    message_ts("Log file opened")
  
  }, error = function(cond) {
    
    message_ts("ERROR - Could not start logger.")
    try(close(log_conn), silent = TRUE)
    stop(cond)
    
  })
  
  # LOGGING
  result <- try({
    withCallingHandlers({
      
      #cat_ts("test", file = log_conn)
      message("foo")
      warning("bar")
      stop("baz")
      return(1)
      
      },
      error = function(cond) {
        cond$traceback <- .traceback(3, max.lines = 10) #.traceback() is silent
        msg <- cond$message
        print_ts("ERROR - ", msg)
        print_ts("DEBUG - Traceback:")
        print_traceback(cond$traceback)
        cat_ts("ERROR - ", msg, file = log_conn)
        print_traceback(cond$traceback, file = log_conn)
        error_glb <<- cond #can't return anything from this portion, so need to set global var
      },
      warning = function(cond) {
        msg <- cond$message
        print_ts("WARN - ", msg)
        cat_ts(msg, file = log_conn)
        invokeRestart("muffleWarning")
      },
      message = function(cond) {
        msg <- sub("\\n$", "", cond$message)
        print_ts("INFO - ", msg)
        cat_ts(msg, file = log_conn)
        invokeRestart("muffleMessage")
      }
    )
    
    # Report success if above code executed
    return(log_json("SUCCESS", "Site evaluation", "test", "evaluate_restoration_site()"))
    
  })
  
  # Report unhandled error
  if (is.try_error(result)) {
    #result_glb <<- result
    msg <- attr(result, "condition")$message
    message_ts("FATAL - ", msg)
    
    # JSON
    return(log_json("ERROR", "foo", msg, error_glb$traceback))
    
  }

  # Close log
  close(log_conn)
  
  readLines(log_file)
  
}

evaluate_restoration_site("test")

# Wrapper with a common handler name
main <- function(in_json) {
  
  return(evaluate_restoration_site(in_json))
  
}