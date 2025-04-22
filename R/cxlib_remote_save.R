#' (Experimental) Utility function to retrieve results from a job
#' 
#' @param x Job reference
#' @param out Output directory path
#' @param queue URL to R Compute Service
#' 
#' @return Invisible job reference
#' 
#' @export


cxlib_remote_save <- function( x, out = base::getwd(), queue = NULL ) {
  
  
  if ( is.null(x) || any(is.na(x)) || ! inherits( x, "character" ) || (length(x) == 0) ) 
    stop( "Job reference missing or invalid" )

  
  # -- determine URL
  px_url <- NA
  
  if ( "url" %in% names(attributes(x)) )
    px_url <- attributes(x)[["url"]]
  
  if ( is.na(px_url) && ! is.null(queue) ) 
    px_url <- queue
  
  if ( is.na(px_url) )
    stop( "No processing queue defined" )
  
  

  # -- output path
  
  xpath <- cxlib::cxlib_standardpath(out) 
  
  if ( ! dir.exists(xpath) )
    stop( "Output desitination does not exist" )
  
  
      
  # -- retrieve results
  
  rslt_results <- httr2::request( px_url ) |>
    httr2::req_url_path( paste0( "/api/job/", x, "/results") ) |>
    httr2::req_method("GET") |>
    httr2::req_auth_bearer_token( cxlib:::.cxlib_remote_accesstoken() ) |>
    httr2::req_perform()
  
  
  if ( rslt_results$status_code %in% c( 401, 403 ) )
    stop( "Authentication failed" )
  
  if ( rslt_results$status_code == 409 )
    stop( "Job has not completed processing" )
  
  
  if ( rslt_results$status_code != 200 ) 
    stop( "Internal error" )

  
  # -- results archive
  
  # - temporary file
  tmp_results <- cxlib::cxlib_standardpath( base::tempfile( pattern = "job-results-", tmpdir = base::tempdir(), fileext = ".zip" ) )
  base::writeBin( httr2::resp_body_raw(rslt_results), con = tmp_results )

  if ( ! file.exists(tmp_results) )
    stop( "Could not retrieve results")


  # - extract archive
  zip::unzip( tmp_results, overwrite = TRUE, exdir = out )
  
  file.remove( tmp_results )
  
  
  
  return(invisible(x))
}