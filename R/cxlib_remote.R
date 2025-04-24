#' (Experimental) Utility function to run one or more programs in a remote queue
#' 
#' @param x Vector of programs
#' @param label Job reference label
#' @param options Job options
#' @param queue URL to R Compute Service
#' 
#' @return A Job ID with attributes
#' 
#' @description
#' The utility function submits a vector of programs to run remotely.
#' 
#' The `label` is a simple reference label that can be used to identify a job.
#' 
#' `options` can be used to override default options defined through cxlib 
#' properties (see \link[cxlib]{cxlib_batch}).
#' 
#' The returned object is a Job ID with connection attributes.
#' 
#' 
#' 
#' @export


cxlib_remote <- function( x, label = NULL, options = NULL, queue = NULL ) {

    
  if ( is.null(x) || any(is.na(x)) || ! inherits( x, "character" ) || (length(x) == 0) )
    stop( "Programs missing or invalid" )

  if ( is.null(queue) || any(is.na(queue)) || ! inherits( queue, "character" ) || (length(queue) != 1) )
    stop( "Process queue missing or invalid" )
  
  
  # -- library configuration
  lib_cfg <- cxlib::cxlib_config()
  
  

  # -- process queue URL
  px_url <- queue
  
  
  # -- standardize program paths
  xpath_pgms <- cxlib::cxlib_standardpath( x )
  
  

  # -- generate job configuration
  #    note: hack for now 
  
  job_cfg <- list( "mode" = "source",
                   "programs" = xpath_pgms )
  
  if ( ! is.null(options) )
    job_cfg[["options"]] <- options
  
  
  job_obj <- cxlib::cxlib_batchjob( job_cfg )

  # - capture configured options
  job_opts <- job_obj$.attr[["options"]]
  
  
    
  
  # -- generate archive
  
  archive_file <- cxlib::cxlib_standardpath( base::tempfile( pattern = "job-archive-", tmpdir = base::tempdir(), fileext = ".zip") )
  
  incl_files <- list.files( job_obj$.attr[["paths"]]["work.area"], recursive = TRUE, include.dirs = FALSE, all.files = TRUE )
  
  if ( ! file.exists( zip::zip( archive_file, incl_files, root = job_obj$.attr[["paths"]]["work.area"] ) ) )
    stop( "Could not prepare job archive" )

  # - drop job 
  job_obj$delete()
  
  
  
  
  # -- job definition
  
  job_def <- list( "label" = paste( "Job created", format( Sys.time(), format = "%Y-%m-%d %H:%M %Z" )), 
                   "options" = job_opts )

  
  # - add label
  if ( ! is.null(label)  )
    job_def[["label"]] <- paste( base::trimws(as.character(label)), paste0("(created ", format( Sys.time(), format = "%Y-%m-%d %H:%M %Z" ), ")") )
  
  

  
  # -- post job request to remote queue
 
  rslt_reg <- httr2::request( px_url ) |>
    httr2::req_url_path("/api/job") |>
    httr2::req_method("POST") |>
    httr2::req_options( ssl_verifypeer = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE), 
                        ssl_verifyhost = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE) ) |>    
    httr2::req_auth_bearer_token( cxlib:::.cxlib_remote_accesstoken() ) |>
    httr2::req_body_json( job_def ) |>
    httr2::req_perform()
  
  
  if ( rslt_reg$status_code %in% c( 401, 403 ) )
    stop( "Authentication failed" )
  
  
  if ( rslt_reg$status_code != 201 )
    stop("An internal error occurred" )
  
  
  lst_reg <- httr2::resp_body_json( rslt_reg ) 
  
  
  if ( ! "id" %in% names(lst_reg) )
    stop( "Expected job ID was not returned")

  jid <- unname(lst_reg[["id"]])
  
  
  
  # -- add archive contents to job request
  
  rslt_arch <- httr2::request(px_url) |>
    httr2::req_url_path( paste0("/api/job/", jid) ) |>
    httr2::req_method("PUT") |>
    httr2::req_options( ssl_verifypeer = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE), 
                        ssl_verifyhost = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE) ) |>    
    httr2::req_auth_bearer_token( cxlib:::.cxlib_remote_accesstoken() ) |>
    httr2::req_headers( "Content-Type" = "application/octet-stream") |>
    httr2::req_body_file( archive_file ) |> 
    httr2::req_perform()
  
  if ( rslt_arch$status_code != 200 )
    stop("An internal error occurred" )
  
  
  # -- register program actions with job request
  
  rslt_pgms <- httr2::request(px_url) |>
    httr2::req_url_path( paste0("/api/job/", jid, "/actions" ) ) |>
    httr2::req_method("PATCH") |>
    httr2::req_options( ssl_verifypeer = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE), 
                        ssl_verifyhost = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE) ) |>    
    httr2::req_auth_bearer_token( cxlib:::.cxlib_remote_accesstoken() ) |>
    httr2::req_body_json( xpath_pgms ) |>
    httr2::req_perform()
    
  if ( rslt_pgms$status_code != 200 )
    stop("Registering program actions failed" )
  

  # -- submit job 
  
  rslt_commit <- httr2::request(px_url) |>
    httr2::req_url_path( paste0("/api/job/", jid, "/submit") ) |>
    httr2::req_method("PUT") |>
    httr2::req_options( ssl_verifypeer = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE), 
                        ssl_verifyhost = lib_cfg$option("REMOTE.VERIFYSSLCERT", unset = TRUE) ) |>    
    httr2::req_auth_bearer_token( cxlib:::.cxlib_remote_accesstoken() ) |>
    httr2::req_perform()
  

  if ( rslt_commit$status_code != 202 )
    stop("Submitting job to process failed" )
  
  
  jrun <- httr2::resp_body_json( rslt_commit )
  
  
  
  # -- job details
  
  jattr <- list( "context" = "rcx.jobreference",
                 "url" = px_url )
  
  if ( "attributes" %in% names(jrun) && "label" %in% names(jrun[["attributes"]]) ) 
    jattr[["label"]] <- unname(jrun[["attributes"]][["label"]])
    
  
  rtrn <- jid
  attributes(rtrn) <- jattr

    
  return(invisible(rtrn))  
}