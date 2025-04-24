#' (Experimental) Utility function to list jobs in a remote queue
#' 
#' @param queue URL to R Compute Service
#' 
#' @return Invisible list of jobs
#' 
#' @description
#' The function will retrieve the list of registered jobs in the process queue
#' 
#' 
#' 
#' @export


cxlib_remote_listjobs <- function( queue = NULL) {

  
  if ( is.null(queue) || any(is.na(queue)) || ! inherits( queue, "character" ) || (length(queue) != 1) )
    stop( "Process queue missing or invalid" )
  
  
  # -- library configuration
  lib_cfg <- cxlib::cxlib_config()
  

  # --- process URL
  px_url <- queue
  
  
  # -- retrieve list of jobs
  rslt <- httr2::request( px_url ) |>
    httr2::req_url_path( "/api/jobs" ) |>
    httr2::req_method("GET") |>
    httr2::req_options( ssl_verifypeer = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE), 
                        ssl_verifyhost = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE) ) |>
    httr2::req_auth_bearer_token( cxlib:::.cxlib_remote_accesstoken() ) |>
    httr2::req_perform()
    
  
  if ( rslt$status_code %in% c( 401, 403 ) )
    stop( "Authentication failed" )
     
  
  if ( rslt$status_code != 200 )
    stop("An internal error occurred" )
  
  
  # -- returned list of jobs
  lst <- httr2::resp_body_json(rslt)
  
  if (length(lst) == 0 ) {
    cat( paste( "No jobs found on", queue), sep = "\n" )
    return(invisible(lst))
  }
  
  
  # -- show jobs
  
  jobs <- character(0)
  
  for ( xitem in lst ) {
  
    if ( ! "id" %in% names(xitem) )
      next()
      
    if ( "attributes" %in% names(xitem) && "label" %in% names(xitem[["attributes"]]) ) {
      jobs <- append( jobs, paste( xitem[["attributes"]][["label"]], paste0("(", xitem[["id"]] ,")"), sep = "  " ) )
    } else {
      jobs <- append(jobs, xitem[["id"]] )
    }
  }
  
  cat( c( " ", 
          paste( "Jobs on", queue ), 
          paste(base::rep_len("-", 60), collapse = ""),
          jobs,
          " " ), 
       sep = "\n" )
  
  
  return(invisible(lst))
}