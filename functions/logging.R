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
print_traceback <- function(x, file = "") {
  
  n <- length(x)
  
  for (i in 1L:n) {
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

# JSON log
# Adapted from lsalas
log_json <- function(result, process = "A process", desc = "Description", call = NULL, timestamp_str = Sys.time()) {
  
  if (!require(jsonlite)) {
    return(noquote(paste0('[{"Result":"ERROR", "Timestamp":"', timestamp_str, '", Process":"JSON logging", 
                   "Description":"JSON formatting requires jsonlite package", "CALL":"log_json()"}]')))
  }  
  
  tryCatch({
    
    if (is.pairlist(call)) call <- as.list(call)
    if (is.list(call)) call <- paste0(unlist(call), collapse = "; ")
    call <- gsub("\\\"", "\\'", call)
    
    tdf <- data.frame(Result = result, 
                      Process = process, 
                      Timestamp = timestamp_str, 
                      Description = desc, 
                      Call = call)
    return(jsonlite::toJSON(tdf))
  },
  error = function(cond) {
    return(noquote(paste0('[{"Result":"ERROR", "Timestamp":"', timestamp_str, 
                          '", Process":"JSON logging", "Description":"Error converting passed log file to JSON; ',
                          cond$message, '", "CALL":"log_json"}]')))
  })

}
evaluate_restoration_site("test")
#log_json("ERROR", alist("foo"), cond_test$message, stop("forced stop"))

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
