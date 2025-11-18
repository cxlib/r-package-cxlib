#' Utility function to save a job definition to a file
#' 
#' @param x A job \link[cxlib]{cxlib_job}
#' @param path File path to save job
#' 
#' @return Invisible logical 
#' 
#' @description
#' Save a job `x` to as a file `path` where `x` is a \link[cxlib]{cxlib_job}
#' 
#' The file represented by `path` does not require a or can implement any
#' file name extension.
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
#' 
#' @export


cxlib_savejob <- function( x, path ) {
  
  
  if ( ! inherits( x, "cxlib_job" ) )
    stop( "The specified job is an invalid type" )
  
  
  if ( ! inherits( path, "character") || (length(path) != 1) || (base::trimws(path) == "") )
    stop( "The file path is missing or invalid" )
  
  
  # -- standardize path
  xpath <- cxlib::cxlib_standardpath( path )

  if ( base::dir.exists(xpath) )
    stop( "The file path is a directory" )

  if ( grepl( "/", xpath ) && ! base::dir.exists(base::dirname(xpath)) )
    stop( "The parent directory for file path does not exist" )
  
  
  job_json <- try( x$toJSON(), silent = FALSE )
  
  if ( inherits( job_json, "try-error" ) )
    stop( "Could not export job to JSON format" )
 
  
  if ( inherits( try( base::writeLines( job_json, con = xpath ), silent = FALSE), "try-error") )
    stop( "Failed to save job to file")
  
  return(invisible(TRUE))
}
