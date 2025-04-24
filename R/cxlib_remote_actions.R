#' (Experimental) Utility function to list actions for a job
#' 
#' @param x Job reference
#' @param queue URL to R Compute Service
#' 
#' @return Invisible job reference
#' 
#' @export


cxlib_remote_actions <- function( x, queue = NULL ) {

  
  if ( is.null(x) || any(is.na(x)) || ! inherits( x, "character" ) || (length(x) == 0) ) 
    stop( "Job reference missing or invalid" )
  
  
  # if ( is.null(x) || any(is.na(x)) || ! inherits( x, "character" ) || (length(x) == 0) ||
  #      ! "context" %in% names(attributes(x)) || ( attributes(x)[["context"]] != "rcx.jobreference") ||
  #      ! "url" %in% names(attributes(x)) ) 
  #   stop( "Job reference missing or invalid" )
  
  
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
  
  

  # -- get actions  

  rslt_actions <- httr2::request( px_url ) |>
    httr2::req_url_path( paste0( "/api/job/", x) ) |>
    httr2::req_method("GET") |>
    httr2::req_options( ssl_verifypeer = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE), 
                        ssl_verifyhost = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE) ) |>    
    httr2::req_auth_bearer_token( cxlib:::.cxlib_remote_accesstoken() ) |>
    httr2::req_perform()
  
  
  if ( rslt_actions$status_code %in% c( 401, 403 ) )
    stop( "Authentication failed" )
  
  
  if ( rslt_actions$status_code != 200 )
    stop("An internal error occurred" )
  
  
  lst_actions <- httr2::resp_body_json(rslt_actions)

  
  
  # -- resolve job details

  x_job <- x 
  
  lst_attr <- list()
  
  if ( ! is.null(attributes(x)) )
    lst_attr <- attributes(x)

  if ( ! "context" %in% names(lst_attr) )
    lst_attr[["context"]] <- "rcx.jobreference"
      
  if ( ! "url" %in% names(lst_attr) )
    lst_attr[["url"]] <- px_url
  
  if ( ! "label" %in% names(lst_attr) && 
       "attributes" %in% names(lst_actions) && "label" %in% names(lst_actions[["attributes"]]) )
    lst_attr[["label"]] <- unname(lst_actions[["attributes"]][["label"]])


  attributes(x_job) <- lst_attr
  
  
    
  # -- display job information
  cxlib::cxlib_remote_jobinfo(x_job)
  
  
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
      

  
  return(invisible(x_job))
}