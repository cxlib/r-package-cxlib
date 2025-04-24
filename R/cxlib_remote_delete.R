#' (Experimental) Delete a job entry in the remote processing node
#' 
#' @param x Job reference
#' @param queue URL to R Compute Service
#' 
#' @return Invisible job reference
#' 
#' @export


cxlib_remote_delete <- function( x, queue = NULL ) {
  
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || ! inherits( x, "character" ) || (length(x) == 0) ) 
    stop( "Job reference missing or invalid" )
  
  
  # -- library configuration
  lib_cfg <- cxlib::cxlib_config()
  
  
  # -- determine URL
  px_url <- NA
  
  if ( "url" %in% names(attributes(x)) )
    px_url <- attributes(x)[["url"]]
  
  if ( is.na(px_url) && ! is.null(queue) ) 
    px_url <- queue
  
  if ( is.na(px_url) )
    stop( "No processing queue defined" )
  

  # -- delete job
  
  rslt_del <- httr2::request( px_url ) |>
    httr2::req_url_path( paste0( "/api/job/", x ) ) |>
    httr2::req_method("DELETE") |>
    httr2::req_options( ssl_verifypeer = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE), 
                        ssl_verifyhost = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE) ) |>    
    httr2::req_auth_bearer_token( cxlib:::.cxlib_remote_accesstoken() ) |>
    httr2::req_perform()
  
  
  if ( rslt_del$status_code %in% c( 401, 403 ) )
    stop( "Authentication failed" )
  
  if ( rslt_del$status_code == 409 )
    stop( "Job has not completed processing" )
  
  
  if ( rslt_del$status_code != 200 ) 
    stop( "Internal error" )
  
  
  cxlib::cxlib_remote_jobinfo(x)
  
  cat( c( " ", "Job successfully deleted" ), sep = "\n" )   

  
  return(invisible(x)) 
}