#' (Experimental) Utility function to induce a timeout period waiting for results
#' 
#' @param x Job reference
#' @param wait Duration in minutes
#' @param queue URL to R Compute Service
#' 
#' @return Invisible job reference
#' 
#' @description
#' A simple utility function to wait for job execution to complete
#' 
#' The `wait` duration in minutes. If the job has not completed before the 
#' duration is exceeded,  
#' 
#' 
#' @export


cxlib_remote_wait <- function( x, wait = 5, queue = NULL ) {
  
  if ( is.null(x) || any(is.na(x)) || ! inherits( x, "character" ) || (length(x) == 0) ||
       ! "context" %in% names(attributes(x)) || ( attributes(x)[["context"]] != "rcx.jobreference") ||
       ! "url" %in% names(attributes(x)) ) 
    stop( "Job reference missing or invalid" )

  
  # -- determine URL
  px_url <- NA
  
  if ( "url" %in% names(attributes(x)) )
    px_url <- attributes(x)[["url"]]
  
  if ( is.na(px_url) && ! is.null(queue) ) 
    px_url <- queue
  
  if ( is.na(px_url) )
    stop( "No processing queue defined" )
  



  # -- first poll
  
  api_path <- paste0( "/api/job/", x ) 
  
  
  rslt_poll <- httr2::request( px_url ) |>
    httr2::req_url_path(api_path) |>
    httr2::req_method("HEAD") |>
    httr2::req_auth_bearer_token( cxlib:::.cxlib_remote_accesstoken() ) |>
    httr2::req_perform()
  
  
  if ( rslt_poll$status_code %in% c( 401, 403 ) )
    stop( "Authentication failed" )
  
  if ( ! rslt_poll$status_code %in% c( 200, 201, 202 ) )
    stop( "Internal error" )
  
  
  # - already done
  if ( rslt_poll$status_code == 200 )
    return(invisible(x))
  
  
  base::rm(rslt_poll)
  
  
  
  # -- initial polling 
  #    note: once every 30 seconds for first 5 minutes 

  for ( xidx in 1:min( 10, 2*wait ) ) {
    
    # - attempt to sleep for 30 seconds
    Sys.sleep(30)
    
    rslt_poll <- httr2::request( px_url ) |>
      httr2::req_url_path(api_path) |>
      httr2::req_method("HEAD") |>
      httr2::req_auth_bearer_token( cxlib:::.cxlib_remote_accesstoken() ) |>
      httr2::req_perform()
    
    # - done
    if ( rslt_poll$status_code == 200 )
      return(invisible(x))
    
    base::rm(rslt_poll)
  }
    

  # -- long duration polling
  #    note: once every 1 minute after first 5 minutes 
  #    note: max wait is 60 minutes, e.g. 55 = 60 -5
  
  poll_cycles <- min( wait - 5, 55)
  
  if ( poll_cycles < 1 )
    stop("Exceeded max wait duration" )
  
  
  for ( xidx in 1:poll_cycles ) {
    
    # - attempt to sleep for 1 minute
    Sys.sleep(60)
    
    rslt_poll <- httr2::request( px_url ) |>
      httr2::req_url_path(api_path) |>
      httr2::req_method("HEAD") |>
      httr2::req_auth_bearer_token( cxlib:::.cxlib_remote_accesstoken() ) |>
      httr2::req_perform()
    
    # - done
    if ( rslt_poll$status_code == 200 )
      return(invisible(x))
    
    base::rm(rslt_poll)
  }
  
  # -- if we get here ... obviously we are still waiting
  
  cxlib::cxlib_remote_jobinfo(x)
  
  stop("Exceeded max wait duration" )
  
}