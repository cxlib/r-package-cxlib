#' (Experimental) Utility function to read a saved job definition from a file
#' 
#' @param path File path to saved job
#' 
#' @return A \link[cxlib]{cxlib_job} object
#' 
#' @description
#' Read a saved job from file `path`
#' 
#' The job definition is saved in JSON file format.
#' 
#' 
#' @examples
#' 
#' # -- save job
#' myjob <- cxlib::cxlib_job()
#' 
#' cxlib_savejob( myjob, "/some/path/to/myjob.job" )
#' 
#' restored_job <- cxlib_readjob( "/some/path/to/myjob.job" )
#' 
#' 
#' @export


cxlib_readjob <- function( path ) {
  
  if ( ! inherits( path, "character") || (length(path) != 1) || (base::trimws(path) == "") || 
       ! file.exists(path) )
    stop( "The specified file is missing, invalud or does not exist" )
  
  
  job_json <- try( base::readLines( path, warn = FALSE ), silent = FALSE )
  
  if ( inherits( lst_job, "try-error") )
    stop( "An error occurred when reading from file path" )

  
  job_obj <- cxlib::cxlib_job()
  
  job_obj$fromJSON( job_json )
  
      
  return(invisible(job_obj))
}

