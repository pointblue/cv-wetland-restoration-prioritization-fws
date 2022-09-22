# Main function
# Author: nelliott
# Help from: lsalas

# Main function for evaluating restoration sites in CVJV
evaluate_restoration_site <- function(in_json, bucket = "pfss-test-bucket", message_level = "INFO") {
  
  # Load code
  #code_dir <- "F:/code/cv-wetland-restoration-prioritization-fws/functions"
  code_files <- list.files(code_dir, full.names = TRUE)
  sourced <- lapply(code_files, FUN = function(x) {
    
    try({
      
      source(x)
      if(message_level %in% c("DEBUG", "TRACE")) message("[", Sys.time(), "] - TRACE - Code file ", basename(x), " sourced")
      return(TRUE)
      
    }, silent = TRUE)
    
  })
  
  # Check for errors in sourcing
  errors <- unlist(lapply(sourced, FUN = function(x) inherits(x, "try-error")))
  if (any(errors)) {
   
    warning("[", Sys.time(), "] - WARNING - Could not source the following files: ", paste0(code_files[errors], collapse = ", "))
    stop(sourced[errors]) 
    
  }
  
  log_file <- file.path(getwd(), "log_file.txt")
  
  # LOGGING
  tryCatch({
    
    # Start log
    result <- tryCatch({
      
      log_conn <- suppressWarnings(file(log_file, "w"))
      message("[", Sys.time(), "] - INFO - Log file opened")
      
    }, error = function(cond) {
      
      cond$level <- "ERROR"
      cond$process <- "Logger initalization"
      cond$traceback <- .traceback(3, max.lines = 10) #.traceback() is silent
      msg <- "Could not start logger; "
      cond$message <- paste(msg, cond$message)
      message("[", Sys.time(), "] - ERROR - ", cond$message)
      #try(close(log_conn), silent = TRUE)
      #error_glb <<- cond #can't return anything from this portion, so need to set global var
      stop(cond)
      
    })
    
    
    withCallingHandlers({
      
      message("Message log level: ", message_level)
      
      #cat_ts("test", file = log_conn)
      process <- "foo"
      message("foo")
      
      process <- "debug"
      message_debug("debug")
      
      process <- "trace"
      message_trace("trace")
      
      process <- "baz"
      warning("baz")
      print("between baz")
      stop("baz")
      return(1)
      
      },
      error = function(cond) {
        
        # Gather info
        cond$level <- "ERROR"
        cond$process <- process
        cond$traceback <- .traceback(3, max.lines = 10) #.traceback() is silent
        
        # Print and log message
        msg <- add_ts(paste0("ERROR - ", cond$message))
        message_report(msg)
        cat(msg, file = log_conn)
        
        # Print and log traceback
        message_report_ts("DEBUG - Traceback:")
        print_traceback(cond$traceback)
        cat_ts("DEBUG - Traceback:", file = log_conn)
        print_traceback(cond$traceback, file = log_conn)
        
        stop(cond)
        
      },
      warning = function(cond) {
        msg <- add_ts(paste0("WARN - During process ", process, "; ", cond$message))
        message_report(msg)
        cat(msg, file = log_conn)
        invokeRestart("muffleWarning")
      },
      DEBUG = function(cond) {
        if (message_level == "DEBUG") {
          msg <- add_ts(paste0("DEBUG - ", sub("\\n$", "", cond$message)))
          message_report(msg)
          cat(msg, file = log_conn)
        }
        invokeRestart("muffleMessage") 
      },
      TRACE = function(cond) {
        if (message_level %in% c("DEBUG", "TRACE")) {
          msg <- add_ts(paste0("TRACE - ", sub("\\n$", "", cond$message)))
          message_report(msg)
          cat(msg, file = log_conn)
        }
        invokeRestart("muffleMessage")
      },
      message = function(cond) {
        if (!inherits(cond, "REPORT")) {
          msg <- add_ts(paste0("INFO - ", sub("\\n$", "", cond$message)))
          message_report(msg)
          cat(msg, file = log_conn)
          invokeRestart("muffleMessage")
        }
      }
    )
    
    # Report success if above code executed
    return(log_json("SUCCESS", "Site evaluation", "test", "evaluate_restoration_site()"))
    
  }, error = function(cond) {
    
    #print(str(cond))
    cond_glb <<- cond
    msg <- cond$message
    message_ts("FATAL - Un-handled error has terminated processing. ", msg)
    
    # Try to write final error message to log (wrap b/c it could be the log that failed)
    try(cat_ts("FATAL - Un-handled error has terminated processing. ", msg, file = log_conn), silent = TRUE)
    
    # Try to close error log
    try(close(log_conn), silent = TRUE)
    
    # Build log name 
    obj <- paste0("logs/log_error_", format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"), ".txt")
    obj <- paste0("logs/log_error_test.txt")
    #obj <- paste0("logs/log_error_cell_", cell, "_month_", mth, "_", format(Sys.time(), format = "%Y-%m-%d_%H-%M-%S"), ".txt")
    log_obj <- file.path("s3:/", bucket, obj)
    
    # Try to copy the error log to the S3 bucket
    log_object <- try({
      
      message_ts("TRACE - Uploading log...")
      put_object(log_file, obj, bucket)
      message_ts("INFO - Log uploaded to ", log_obj)
      log_obj
      
    }, silent = TRUE)
    if (is.try_error(log_object)) {
      
      log_cond <- attr(log_object, "condition")
      msg <- paste0("ERROR - During log file upload to ", log_obj, ": ", log_cond$message)
      message_ts(msg)
      log_object <- msg
      
    }
    
    # JSON
    #print(cond)
    return(log_json("ERROR", 
                    process = cond$process, 
                    desc = cond$message, 
                    call = cond$traceback[[1]], #returning whole call stack via json is very messy
                    log_file = log_object)) 
    
  })
  
}

#evaluate_restoration_site("test")

# Wrapper with a common handler name
main <- function(in_json, ...) {
  
  return(evaluate_restoration_site(in_json, ...))
  
}
