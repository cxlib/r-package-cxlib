#' Internal function to generate a results archive from completed actions
#' 
#' @param x Result archive file
#' @param job.path Path to job actions
#' @param work.path Path to job work area
#' 
#' @return Path to archive file
#' 
#' 

.cxlib_batchjob_resultsarchive <- function( x, job.path = NULL, work.path = NULL ) {
  

  if ( missing(x) || is.null(x) || (length(x) != 1) || ! inherits(x, "character") ) 
    stop( "Results archive file missing")
    

  if ( is.null(job.path) || all(is.na(job.path)) || (length(job.path) != 1) || ! dir.exists(job.path) )
    stop( "Job path missing or invalid")
  
  job_path <- cxlib::cxlib_standardpath( job.path )
  
    
  if ( is.null(work.path) || all(is.na(work.path)) || (length(work.path) != 1) || ! dir.exists(work.path) )
    stop( "Job work area path missing or invalid")
  
  work_area <- cxlib::cxlib_standardpath( work.path )
  
  
  
  
  
  # -- get actions
  
  action_lst <- list()
  
  
  lst_files <- base::sort( list.files( job_path, 
                                       pattern = "^\\d+-action-.*.(json|lck)$", 
                                       recursive = FALSE, include.dirs = FALSE, full.names = TRUE ) )
  
  for ( xfile in lst_files ) {
    
    # xaction <- jsonlite::fromJSON( xfile, flatten = TRUE )
    xaction <- jsonlite::fromJSON( xfile, simplifyDataFrame = FALSE )
    
    xaction[["status"]] <- "planned"
    
    if ( tools::file_ext( xfile) == "lck" )
      xaction[["status"]] <- "executing"
    
    if ( grepl( "-completed.json$", xfile, ignore.case = TRUE, perl = TRUE) )
      xaction[["status"]] <- "completed"
    
    action_lst[[ length(action_lst) + 1 ]] <- xaction
    
  }
  

  
  
  # -- integrity checks and actions completed 
  
  for ( xact in action_lst ) {
    
    # - action should be completed
    if ( ! "status" %in% names(xact) || ! xact[["status"]] %in% c( "complete", "completed" ) )
      stop( "Not all actions have completed")
    
    
    # - program action integrity
    
    if ( "type" %in% names(xact) && (base::toupper(xact[["type"]]) == "PROGRAM") ) {
      
      # program
      
      work_pgm <- file.path( work_area, xact[["path"]], fsep = "/" )
      
      if ( ! file.exists( work_pgm ) )
        stop( "The source for the program ", xact[["path"]], " no longer exists" )
      
      if ( ! "sha1" %in% names(xact) || ( digest::digest( work_pgm, algo = "sha1", file = TRUE ) != xact[["sha1"]] ) )
        stop( "The program ", xact[["path"]], " has changed since submitted or integrity cannot be verified" )

      
      # log
            
      if ( ! "log" %in% names(xact) || ! "path" %in% names(xact[["log"]]) )
        stop( "Expecting log details for a program action" )
      
      
      if ( ! file.exists( file.path( work_area, xact[["log"]][["path"]], fsep = "/" ) ) )
        stop( "Expecting log file to exist as a result of a program action")
      

    }  # end of if-statement for action type program
    
    
  } # end of for-statement across actions for integrity checks and completed actions
  
  base::rm( list = "xact" )
  
  
  
  
  
  # -- initiate file system events to process
  
  files_to_write <- character(0)
  files_to_delete <- character(0)
  
  logs_to_write <- character(0)
  
  
  # -- process actions
  #    note: one action at a time as files created in one may be deleted in another
  
  for ( xact in action_lst ) {
    
    
    # - action log
    if ( "log" %in% names(xact) && "path" %in% names(xact[["log"]]) )
      logs_to_write <- base::unique( append( logs_to_write, xact[["log"]][["path"]] ) )
    
    
    # - look for annotated output locations ... none means nothing to do
    
    if ( ! "annotations" %in% names(xact) || ! "output" %in% names(xact[["annotations"]]) )
      next()
    
    out_anno <- xact[["annotations"]][["output"]]
    
    
    # - process deleted files in annotated output locations
    
    if ( "files.deleted" %in% names(xact) ) {
      
      for ( xtodelete in xact[["files.deleted"]] ) {
        
        # futility ... file not in output location 
        if ( ! base::dirname( xtodelete[["path"]] ) %in% out_anno )
          next()
        
        # register file to be deleted
        files_to_delete <- base::unique( append( files_to_delete, xtodelete[["path"]] ) )
        
        # updated files to write excluding those that were deleted
        files_to_write <- files_to_write[ ! files_to_write %in% files_to_delete ]
        
      }
      
      rm( list = "xtodelete")
    }
    
    
    # - process created or updated files in annotated output locations
    
    for ( xscope in c( "files.created", "files.updated" ) )
      if ( xscope %in% names(xact) ) {
        
        for ( xtowrite in xact[[xscope]] ) {
          
          # futility ... file not in output location 
          if ( ! base::dirname( xtowrite[["path"]] ) %in% out_anno )
            next()
          
          # register file to be written 
          files_to_write <- base::unique( append( files_to_write, xtowrite[["path"]] ) )
          
          # update files to be deleted excluding those that are written
          files_to_delete <- files_to_delete[ ! files_to_delete %in% files_to_write ]
          
        }
        
        rm( list = "xtowrite" ) 
      }
    
    
    rm( list = "out_anno" )
    
  }  # end of for-statement across actions to determine writes and deletes 
  
  
  
  # -- create archive
  
  zip_archive <- zip::zip( x, base::unique( c(logs_to_write, files_to_write)), root = work_area )
  
  return(invisible( zip_archive ))  
  
}

