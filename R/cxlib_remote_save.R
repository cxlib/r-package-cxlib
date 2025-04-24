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
  
  

  # -- output path
  
  xpath <- cxlib::cxlib_standardpath(out) 
  
  if ( ! dir.exists(xpath) )
    stop( "Output desitination does not exist" )
  
  
      
  # -- retrieve results
  
  rslt_results <- httr2::request( px_url ) |>
    httr2::req_url_path( paste0( "/api/job/", x, "/results") ) |>
    httr2::req_method("GET") |>
    httr2::req_options( ssl_verifypeer = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE), 
                        ssl_verifyhost = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE) ) |>    
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

  
  # -- perform pre-extract inventory
  pre_inv <- sapply( cxlib::cxlib_standardpath( list.files( out, recursive = TRUE, all.files = TRUE, include.dirs = FALSE ) ), function(x) {
    digest::digest( file.path( out, x, fsep = "/"), algo = "sha1", file = TRUE )
  })
  

  # - extract archive
  zip::unzip( tmp_results, overwrite = TRUE, exdir = out )

  file.remove( tmp_results )
  
  
  # -- perform pre-extract inventory
  post_inv <- sapply( cxlib::cxlib_standardpath( list.files( out, recursive = TRUE, all.files = TRUE, include.dirs = FALSE ) ), function(x) {
    digest::digest( file.path( out, x, fsep = "/"), algo = "sha1", file = TRUE )
  })


  # -- document save location

  rslt_info <- c( " ", 
                  paste( "Job results saved to", cxlib::cxlib_standardpath( out ) ),
                  paste( rep_len("-", 60), collapse = "") )

  for ( xfile in sort(union( names(pre_inv), names(post_inv) )) ) {
    
    # - nothing done to the file
    if ( xfile %in% names(pre_inv) && xfile %in% names(post_inv) &&
         pre_inv[xfile] == post_inv[xfile] )
      next()
      
    
    xstate <- character(0)

    
    if ( ! xfile %in% names(pre_inv) && xfile %in% names(post_inv) ) 
      xstate <- "new"
    
    if ( xfile %in% names(pre_inv) && xfile %in% names(post_inv) ) 
      xstate <- "updated"
    
    if ( xfile %in% names(pre_inv) && ! xfile %in% names(post_inv) ) 
      xstate <- "deleted"

    xhash <- ifelse( xstate %in% c( "new", "updated"), post_inv[xfile], pre_inv[xfile] )

    
    rslt_info <- append( rslt_info, 
                         c( " ", 
                            paste( xfile, paste0("(", xstate, ")"), sep = "  " ),
                            paste0( "(SHA-1 ", xhash, ")") ) )

  }
  
  cat( c( rslt_info, " "), sep = "\n" )
  
  
  
  # -- resolve job details
  
  x_job <- x 
  
  lst_attr <- list()
  
  if ( ! is.null(attributes(x)) )
    lst_attr <- attributes(x)
  
  if ( ! "context" %in% names(lst_attr) )
    lst_attr[["context"]] <- "rcx.jobreference"
  
  if ( ! "url" %in% names(lst_attr) )
    lst_attr[["url"]] <- px_url
  
  
  attributes(x_job) <- lst_attr
  

  return(invisible(x_job))
}