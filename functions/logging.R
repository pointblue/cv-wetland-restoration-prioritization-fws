# Logging functions

# Basic logging function to add a timestamp to strings
add_ts <- function(...) paste0("[", Sys.time(), "] - ", ...)

# Add as wrapper for base 'message' function
message_ts <- function(...) message(add_ts(), ...)

# Add as wrapper for base 'print' function
print_ts <- function(...) print(add_ts(...))

# Add as a wrapper for base 'cat' function
cat_ts <- function(...) cat(add_ts(), ..., "\n")

# Function to nicely print the results from the R traceback() function
# Taken verbatim from the traceback function
print_traceback <- function(x, ncalls = Inf, file = "") {
  
  n <- length(x)
  if (!is.numeric(ncalls)) ncalls <- Inf
  
  for (i in 1L:min(n, ncalls)) {
    xi <- x[[i]]
    label <- paste0(n - i + 1L, ": ")
    m <- length(xi)
    srcloc <- if (!is.null(srcref <- attr(xi, "srcref"))) {
      srcfile <- attr(srcref, "srcfile")
      paste0(" at ", basename(srcfile$filename), "#", 
             srcref[1L])
    }
    if (isTRUE(attr(xi, "truncated"))) {
      xi <- c(xi, " ...")
      m <- length(xi)
    }
    if (!is.null(srcloc)) {
      xi[m] <- paste0(xi[m], srcloc)
    }
    if (m > 1) 
      label <- c(label, rep(substr("          ", 1L, 
                                   nchar(label, type = "w")), m - 1L))
    cat(paste0(label, xi), sep = "\n", file = file)
  }
  
  
}

# Fix length of variables for log_json
fix_length <- function(x, varname, replacement = "UNKNOWN", check_replacement = TRUE) {
  
  #print(varname)
  #print(x)
  #print(length(x))
  
  if (check_replacement) {
    replacement <- fix_length(replacement, "replacement", "UNKNOWN", FALSE)
    #message("replacement value: ", replacement)
  }
  
  if (length(x) == 0) {
    message("Passed '", varname, "' has length == 0. Replacing with ", as.character(replacement))
    return(replacement)
  } else if (length(x) > 1) {
    message("Passed '", varname, "' has length > 1. Using first element only.")
    return(x[1])
  } else {
    return(x)
  }
  
}

# JSON log
# Adapted from lsalas
log_json <- function(result, process = "A process", desc = "Description", 
                     call = NULL, log_file = NULL, timestamp_str = Sys.time()) {
  
  if (!require(jsonlite)) {
    return(noquote(paste0('[{"Result":"ERROR", "Timestamp":"', timestamp_str, '", Process":"JSON logging", 
                   "Description":"JSON formatting requires jsonlite package", "Call":"log_json()", "LogFile":""}]')))
  }  
  
  tryCatch({
    
    result <- fix_length(result, "result")
    process <- fix_length(process, "process")
    
    desc <- fix_length(desc, "desc")
    
    if (!is.null(call)) {
      if (is.pairlist(call)) call <- as.list(call)
      if (is.list(call)) call <- paste0(unlist(call), collapse = "; ")
      call <- gsub("\\\"", "\\'", call)
      
    }
    call <- fix_length(call, "call", replacement = "")
    log_file <- fix_length(log_file, "log_file", replacement = "")
    timestamp_str <- fix_length(timestamp_str, "timestamp_str", Sys.time())
    
    tdf <- data.frame(Result = result, 
                      Process = process, 
                      Timestamp = timestamp_str, 
                      Description = desc, 
                      Call = call,
                      LogFile = log_file)
    #print(tdf)
    return(jsonlite::toJSON(tdf))
  },
  error = function(cond) {
    try({
      
      message("result: ", result)
      message(length(result))
      message("process: ", process)
      message(length(process))
      message("desc: ", desc)
      message(length(desc))
      message("timestamp_str: ", timestamp_str)
      message(length(timestamp_str))
      message("call: ", call)
      message(length(call))
      
    }, silent = TRUE)
    return(noquote(paste0('[{"Result":"ERROR", "Timestamp":"', timestamp_str, 
                          '", Process":"JSON logging", "Description":"Error converting passed log info to JSON; ',
                          cond$message, '", "Call":"log_json", "LogFile":""}]')))
  })

}

#log_json("ERROR", alist("foo"), cond_test$message, stop("forced stop"))

# Condition constructor
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call),
    ...
  )
  
}
is.condition <- function(x) inherits(x, "condition")

# Lower levels of messages
cond_debug <- function(msg, call = sys.call(-1)) {
  
  structure(
    class = c("DEBUG", "message", "condition"),
    list(message = msg, call = call)
  )
  
}
message_debug <- function(..., call = sys.call(-1), domain = NULL, appendLF = TRUE) {
  
  msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
  message(cond_debug(msg, call = call))
  
}

cond_trace <- function(msg, call = sys.call(-1)) {
  
  structure(
    class = c("TRACE", "message", "condition"),
    list(message = msg, call = call)
  )
  
}
message_trace <- function(..., call = sys.call(-1), domain = NULL, appendLF = TRUE) {
  
  msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
  message(cond_trace(msg, call = call))
  
}

cond_report <- function(msg, call = sys.call(-1)) {
  
  structure(
    class = c("REPORT", "message", "condition"),
    list(message = msg, call = call)
  )
  
}
message_report <- function(..., call = sys.call(-1), domain = NULL, appendLF = TRUE) {
  
  msg <- .makeMessage(..., domain = domain, appendLF = appendLF)
  message(cond_report(msg, call = call))
  
}
message_report_ts <- function(...) message_report(add_ts(), ...)

# Test if an object is an error
is.try_error <- function(x) inherits(x, "try-error")

# Add an error handler
log_ts <- function(...) {}


printmessage_ts <- function(..., type = "message") {
  if (type == "message") {
    message_ts(...)
  } else {
    print_ts(...)
  }
}
