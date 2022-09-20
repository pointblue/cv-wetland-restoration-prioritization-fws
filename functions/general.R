# General functions

# Function to get the nth subelement from each item in a list
extract_subelement <- function(x, element) sapply(x, `[[`, element) 

# Function to make a string filename safe
clean_string <- function(x, sub_char = "-", ...) {
  y <- gsub("[^a-zA-Z0-9_\\-]", sub_char, x, ...)
  
  # Condense multiple sub_chars
  y <- gsub(paste0(sub_char, "+"), sub_char, y, ...)
  
  # Drop terminal sub_char
  y <- gsub(paste0(sub_char, "$"), "", y, ...)
  
  return(y)
}

# Function to make a string filename safe
clean_string_remove_underscores <- function(x, sub_char = "-", ...) {
  y <- gsub("[^a-zA-Z0-9\\-]", sub_char, x, ...)
  
  # Condense multiple sub_chars
  y <- gsub(paste0(sub_char, "+"), sub_char, y, ...)
  
  # Drop terminal sub_char
  y <- gsub(paste0(sub_char, "$"), "", y, ...)
  
  return(y)
}

# Function to split a vector into n chunks of equal size
chunk <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
