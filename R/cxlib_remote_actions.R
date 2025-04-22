#' (Experimental) Utility function to list actions for a job
#' 
#' @param x Job reference
#' @param queue URL to R Compute Service
#' 
#' @return Invisible job reference
#' 
#' @export


cxlib_remote_actions <- function( x, queue = NULL ) {
  
  
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
  
  
  rslt_actions <- httr2::request( px_url ) |>
    httr2::req_url_path( paste0( "/api/job/", x) ) |>
    httr2::req_method("GET") |>
    httr2::req_auth_bearer_token( cxlib:::.cxlib_remote_accesstoken() ) |>
    httr2::req_perform()
  
  
  if ( rslt_actions$status_code %in% c( 401, 403 ) )
    stop( "Authentication failed" )
  
  
  if ( rslt_actions$status_code != 200 )
    stop("An internal error occurred" )
  
  
  lst_actions <- httr2::resp_body_json(rslt_actions)
  
  
  # -- display job information
  
  cxlib::cxlib_remote_jobinfo(x)
  
  
  # - display actions
  
  lst <- character(0)
  
  if ( length(lst_actions[["actions"]]) == 0 )
    cat( "No actions", sep = "\n" )
  
  if ( length(lst_actions[["actions"]]) > 0)
    for ( xidx in 1:length(lst_actions[["actions"]]) ) 
      lst <- append( lst, c( paste( paste0( "Action #", as.character(xidx)) ), 
                             paste( rep_len("-", 70), collapse = ""),
                             lst_actions[["actions"]][[xidx]][["path"]], 
                             " ",
                             paste0( "SHA-1 :  ", lst_actions[["actions"]][[xidx]][["sha1"]] ),
                             paste0( "Log   :  ", lst_actions[["actions"]][[xidx]][["log"]] ), 
                             " ",
                             paste0( "Status:  ", lst_actions[["actions"]][[xidx]][["status"]] ), 
                             " ", " " ) )
                     
      
  cat( c( " ", lst, " "), sep = "\n" )              
      

  
  return(invisible(x))
}