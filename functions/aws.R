# AWS functions

# Functions for interacting with data in Amazon buckets

# Function to check that required access key data are available
check_env_vars <- function(env_vars = c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY"), raise_error = FALSE) {
  
  if (!is.character(env_vars)) stop("Argument 'env_vars' must be a character vector")
  
  env_vars_exist <- unlist(lapply(env_vars, FUN = function(x) { nchar(Sys.getenv(x)) > 0 }))
  
  if (any(!env_vars_exist)) {
    
    msg <- paste0("The following environment variable(s) were not found: ", 
                  paste0(env_vars[!env_vars_exist], collapse = ", "))
    
    if (raise_error == TRUE) {
      stop(msg)
    } else {
      warning(msg)
    }
    
  }
  
  names(env_vars_exist) <- env_vars
  return(env_vars_exist)
  
}

# Vectorized version of function to check that expected buckets can be seen
check_buckets <- function(buckets, raise_error = FALSE, ...) {
  
  # Check inputs
  if (!require(aws.s3)) stop("The package aws.s3 is required but missing")
  if (!is.character(buckets)) stop("Argument 'buckets' must be a character vector")
  if (!is.logical(raise_error)) stop("Argument 'raise_error' must be logical")
  
  bkt_status <- lapply(buckets, bucket_exists, raise_error = raise_error, ...)
  bkt_found <- unlist(extract_subelement(bkt_status, 1))
  
  if (!all(bkt_found)) {
    
    msg <- paste0("The following bucket(s) were not found: ", 
                  paste0(buckets[!bkt_found], collapse = ", "))
    
    if (raise_error == TRUE) {
      stop(msg)
    } else {
      warning(msg)
    }
      
  } 
  
  names(bkt_status) <- buckets
  return(bkt_status)
  
}
#check_buckets("pfss-test-bucket", verbose = TRUE)

# Vectorized version of function to check that expected objects exist in an Amazon S3 bucket
# Note that it can't check for the existence of pseudo-folders, returning FALSE
check_objects <- function(objects, bucket, raise_error = FALSE, ...) {
  
  # Check inputs
  if (!require(aws.s3)) stop("The package aws.s3 is required but missing")
  if (!is.character(bucket)) stop("Argument 'buckets' must be a character")
  if (length(bucket) != 1) stop("Argument 'buckets' must be a character vector of length 1")
  if (!is.character(objects)) stop("Argument 'objects' must be a character vector")
  
  # Check existence of bucket
  bkt_status <- check_buckets(bucket, raise_error = raise_error, ...)
  
  # Check existence of files
  obj_status <- lapply(objects, head_object, bucket = bucket, ...)
  
  obj_found <- unlist(extract_subelement(obj_status, 1))
  
  if (!all(obj_found)) {
    
    msg <- paste0("The following objects(s) were not found in bucket ", bucket, ": ", 
                  paste0(objects[!obj_found], collapse = ", "))
    
    if (raise_error == TRUE) {
      stop(msg)
    } else {
      warning(msg)
    }
    
  }
  
  names(obj_status) <- objects
  return(obj_status)
  
}
#check_objects("test.txt", "pfss-test-bucket", verbose = TRUE)

# Vectorized version of function to save objects from an Amazon S3 bucket
save_objects <- function(objects, bucket, local_dirs, raise_error = FALSE, skip_missing = FALSE, overwrite = FALSE, ...) {
  
  # Check inputs (package, bucket, and objects checked in check_objects)
  if (!require(aws.s3)) stop("The package aws.s3 is required but missing")
  if (!is.character(local_dirs)) stop("Argument 'local_dirs' must be a character vector")
  if (length(local_dirs) != 1 & length(local_dirs) != length(objects)) stop("Argument 'local_dirs' must be either of length 1 or of equal length to 'objects'")
  if (length(local_dirs) == 1) local_dirs <- rep(local_dirs, length(objects))
  
  paths_exist <- file.exists(local_dirs)
  if (!all(paths_exist)) stop("Missing the following local path(s) specified in files: ", paste0(local_dirs[!paths_exist],  collapse = ", "))
  
  if (!is.logical(skip_missing)) stop("Argument 'skip_missing' must be logical")
  if (!is.logical(overwrite)) stop("Argument 'overwrite' must be logical")
  
  # Check objects exist in bucket
  obj_status <- check_objects(objects, bucket, raise_error = raise_error, ...)
  objects_exist <- unlist(extract_subelement(obj_status, 1))
  
  if (!all(objects_exist)) {
    
    if (skip_missing == TRUE) {
      
      if (sum(objects_exist) == 0) {
        warning("None of the specified files exist.")
        return(rep(FALSE, length(objects)))
      } else {
        warning("Will skip missing objects and download others in list.")
      }
      
    } else {
      
      warning("Will skip all objects because some were missing.")
      return(rep(FALSE, length(objects)))
      
    }
    
  }
  
  # Check local file existence
  files_exist <- file.exists(file.path(local_dirs, objects))
  if (any(files_exist)) {
    
    if (overwrite == TRUE) {
      message("The follwoing file(s) already exist and will be overwritten (overwrite == TRUE): ", 
              paste0(objects[files_exist], collapse = ", "))
    } else {
      
      message("Skipping the following file(s) because they already exist and overwrite != TRUE: ", 
              paste0(objects[files_exist], collapse = ", "))
      
    }
    
  }
  
  # Download only if object exists and either the local file doesn't exist or overwrite == TRUE
  download_objects <- objects_exist & (!files_exist | overwrite)
  print(download_objects)
  
  save_status <- mapply(obj = objects, dir = local_dirs, dwn = download_objects, SIMPLIFY = FALSE, FUN = function(obj, dir, dwn, ...) {
    
    if (dwn) {
      try({
        file <- file.path(dir, obj)
        #message(paste0("Downloading ", obj, " to ", dir))
        save_object(obj, bucket, file = file)
        message(paste0(obj, " saved to ", dir))
        return(file)
      }, silent = TRUE)
    } else {
      return(FALSE)
    }
      
  })
  
  errors <- unlist(lapply(save_status, FUN = is.try_error))
  if (any(errors)) {
    
    warning("Could not save the following files: ", paste0(objects[errors], collapse = ", "))
    
    if (raise_error == TRUE) {
      stop(save_status[errors])
    } else {
      warning(save_status[errors])
    }
    
  }
  
  names(save_status) <- objects
  return(save_status)
 
}

#save_objects(c("test.txt", "test.txt"), "pfss-test-bucket", c(tempdir(), getwd()), overwrite = TRUE, verbose = TRUE)


# Vectorized version of function to put objects in an Amazon S3 bucket
put_objects <- function(files, objects, bucket, raise_error = FALSE, skip_missing = FALSE, overwrite = FALSE, ...) {
  
  # Check inputs (package, bucket, and objects checked in check_objects)
  if (!require(aws.s3)) stop("The package aws.s3 is required but missing")
  if (!is.character(files)) stop("Argument 'files' must be a character vector")
  if (length(objects) != 1 & length(objects) != length(files)) stop("Argument 'objects' must be either of length 1 or of equal length to 'files'")
  if (length(objects) == 1) objects <- rep(objects, length(objects))
  
  if (!is.logical(skip_missing)) stop("Argument 'skip_missing' must be logical")
  if (!is.logical(overwrite)) stop("Argument 'overwrite' must be logical")
  
  # Check file existence
  files_exist <- file.exists(files)
  if (!all(files_exist)) {
    msg <- paste0("Missing the following local file(s) specified in files: ", paste0(files[!paths_exist],  collapse = ", "))
    if (skip_missing != TRUE) {
      if (raise_error == TRUE) {
        stop(msg)
      } else {
        warning(msg)
        return(rep(FALSE, length(files)))
      }
    } else {
      warning(msg, ".\n Will attempt to upload non-missing files")
    }
  }
  
  # Check bucket existence
  if (!bucket_exists(bucket)) stop("Specified 'bucket' ", bucket, " not found.")
  
  # Check objects exist in bucket
  obj_status <- check_objects(objects, bucket, raise_error = raise_error, ...)
  objects_exist <- unlist(extract_subelement(obj_status, 1))
  
  if (!all(objects_exist)) {
    
    if (skip_missing == TRUE) {
      
      if (sum(objects_exist) == 0) {
        warning("None of the specified files exist.")
        return(rep(FALSE, length(objects)))
      } else {
        warning("Will skip missing objects and download others in list.")
      }
      
    } else {
      
      warning("Will skip all objects because some were missing.")
      return(rep(FALSE, length(objects)))
      
    }
    
  }
  
  # Check local file existence
  files_exist <- file.exists(file.path(local_dirs, objects))
  if (any(files_exist)) {
    
    if (overwrite == TRUE) {
      message("The follwoing file(s) already exist and will be overwritten (overwrite == TRUE): ", 
              paste0(objects[files_exist], collapse = ", "))
    } else {
      
      message("Skipping the following file(s) because they already exist and overwrite != TRUE: ", 
              paste0(objects[files_exist], collapse = ", "))
      
    }
    
  }
  
  # Download only if object exists and either the local file doesn't exist or overwrite == TRUE
  download_objects <- objects_exist & (!files_exist | overwrite)
  print(download_objects)
  
  save_status <- mapply(obj = objects, dir = local_dirs, dwn = download_objects, SIMPLIFY = FALSE, FUN = function(obj, dir, dwn, ...) {
    
    if (dwn) {
      try({
        file <- file.path(dir, obj)
        #message(paste0("Downloading ", obj, " to ", dir))
        save_object(obj, bucket, file = file)
        message(paste0(obj, " saved to ", dir))
        return(file)
      }, silent = TRUE)
    } else {
      return(FALSE)
    }
    
  })
  
  errors <- unlist(lapply(save_status, FUN = is.try_error))
  if (any(errors)) {
    
    warning("Could not save the following files: ", paste0(objects[errors], collapse = ", "))
    
    if (raise_error == TRUE) {
      stop(save_status[errors])
    } else {
      warning(save_status[errors])
    }
    
  }
  
  names(save_status) <- objects
  return(save_status)
  
}

