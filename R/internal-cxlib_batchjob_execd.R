#' Internal function to execute actions of a batch job
#' 
#' @param x Directory path to job actions
#' @param work Work area directory 
#' 
#' @return Invisible NULL
#' 
#' @description
#' The internal routine is in effect a runner deamon that can be executed as 
#' either a foreground process (wait for the routine to end) or a background
#' process (do not wait for it to end).
#' 
#' The routine uses the `*-action-*` files in the directory specified by `x`. 
#' The action file is a JSON data file with action attributes and directives.
#' 
#' The natural sort order of the action files is the action queue. The queue of 
#' actions are processed one at a time sequentially , e.g. `001-action-*` is 
#' processed before `002-action-*`.
#' 
#' A file that ends `*.json` is an action that is queued and not started or 
#' completed.
#' 
#' A file that ends `*.lck` is an action that is currently being executed and 
#' not completed.
#' 
#' A file that ends `*-completed.json` is an action that is completed.
#' 
#' The routine is persistent and will continue to execute until all actions have
#' been completed.
#' 
#' Signal interrupts are currently not supported.
#' 
#' @keywords internal

.cxlib_batchjob_execd <- function( x, work = NULL ) {
  
  # -- constants
  supported_actions <- c( "PROGRAM" )
  
  
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || ! inherits( x, "character") || ! dir.exists(x) )
    stop( "Directory path to actions missing or invalid")

  
  if ( is.null(work) || any(is.na(work)) || ! inherits( work, "character") || ! dir.exists(work) )
    stop( "Work area directory path missing or invalid")
  
  
  # -- job definition
  
  if ( ! file.exists( file.path( x, "job.json", fsep = "/" ) ) )
    stop( "Job definition not available" )
  
  job_def <- try( jsonlite::fromJSON( file.path( x, "job.json", fsep = "/" ) ), silent = TRUE )
  
  if ( inherits( job_def, "try-error" ) )
    stop( "Could not import job definition")
  
  
  if ( ! "id" %in% names(job_def) )
    stop( "Job ID not defined" )

  if ( ! uuid::UUIDvalidate(job_def[["id"]]) )  
    stop( "Job ID is in an invalid format" )
  
  
  job_def[["paths"]] <- list( ".internal" = cxlib::cxlib_standardpath(x), 
                              "work.area" = cxlib::cxlib_standardpath(work) )
  

  job_def[["start"]] <- format( as.POSIXct( base::Sys.time(), tz = "UTC"), format = "%Y%m%dT%H%M%S")
  job_def[["complete"]] <- NA

  
  # - update job.json
  base::writeLines( jsonlite::toJSON( job_def, pretty = TRUE ), 
                    con = file.path( x, "job.json", fsep = "/" ) )
  
  
  
  # -- get list of action definitions

  act_json <- list.files( x, pattern = "^\\d+-action-.*.(json|lck)$", full.names = FALSE, recursive = FALSE, include.dirs = FALSE )

  if ( any(grepl( "(\\.lck|\\-completed.json)$", act_json, perl = TRUE, ignore.case = TRUE) ) )
    stop( "Specified job is active or stale" )
  
  
  
  # -- process actions
  
  for ( xaction in act_json ) {
  
    # - import action definition
    act_file <- file.path( job_def[["paths"]][[".internal"]], xaction, fsep = "/" )
    
    act_def <- try( jsonlite::fromJSON( act_file ), silent = TRUE )
    
    if ( inherits( act_def, "try-error" ) )
      stop( "Failed to import action ", xaction )
    
    
    
    # - supported action
    
    if ( ! "type" %in% names(act_def) )
      stop( "Could not determine job action type" )
    
    if ( ! base::toupper(act_def[["type"]]) %in% supported_actions )
      stop( "Action ", base::toupper(act_def[["type"]]), " not supported" )
    
    

    # - note action initiate and complete 
    
    act_def[["start"]] <- format( as.POSIXct( base::Sys.time(), tz = "UTC"), format = "%Y%m%dT%H%M%S")
    act_def[["end"]] <- NA
    
    
    # - move action to ongoing
    
    lck_file <- paste0( tools::file_path_sans_ext( act_file ), ".lck" )
    
    if ( inherits( try( file.rename( act_file, lck_file ), 
                        silent = TRUE ), 
                   "try-error" ) )
      stop( "Could not update job action status to executing" )

    
    # - update action 
    if ( inherits( try( writeLines( jsonlite::toJSON( act_def, pretty = TRUE ), 
                                    con = lck_file ),
                        silent = TRUE ), 
                   "try-error" ) )
      stop( "Could not update job action" )


    
    
    # - action type of PROGRAM
        
    if ( base::toupper(act_def[["type"]]) == "PROGRAM" ) {
    
      # set up options
      act_opts <- list( "job.id" = unname(job_def[["id"]]), 
                        "work.area" = unname(job_def[["paths"]][["work.area"]]) )
      
      
      # run program

      act_result <- try( cxlib:::.cxlib_batchjob_execd_program( act_def, act_opts ), silent = FALSE )  
      
      if ( inherits( act_result, "try-error" ) )
        stop( "An internal occurred when running program ", act_def[["path"]] )
      
      
      # capture results
      
      act_def[["log"]][["sha1"]] <- unname(act_result[["log"]][["sha1"]])
      
      for ( xitem in c( "start", "end", "files.input", "files.created", "files.updated", "files.deleted") )
        act_def[ xitem ] <- as.list( act_result[xitem] )

    }  # end of if-statement for type PROGRAM

    
              
    
    # - update action with completion details
    
    act_def[["end"]] <- format( as.POSIXct( base::Sys.time(), tz = "UTC"), format = "%Y%m%dT%H%M%S")
    
    
    if ( inherits( try( writeLines( jsonlite::toJSON( act_def, pretty = TRUE ), 
                                    con = lck_file ),
                        silent = TRUE ), 
                   "try-error" ) )
      stop( "Could not update completed job action" )
    
    
    # - move action to completed
    if ( inherits( try( file.rename( lck_file, 
                                     paste0( tools::file_path_sans_ext( lck_file ), "-completed.json") ), 
                        silent = TRUE ), 
                   "try-error" ) )
      stop( "Could not update job action status to completed" )
    
    
    # - reset
    base::rm( list = c( "act_file", "act_def", "act_result" ) )
    
  }  # end of for-statement on act_json

  

  # -- update job definition
  
  job_def[["complete"]] <- format( as.POSIXct( base::Sys.time(), tz = "UTC"), format = "%Y%m%dT%H%M%S")

  
  # - update job.json
  base::writeLines( jsonlite::toJSON( job_def[ ! names(job_def) %in% "paths" ], pretty = TRUE ), 
                    con = file.path( x, "job.json", fsep = "/" ) )
  
 
  return(invisible(TRUE))
}