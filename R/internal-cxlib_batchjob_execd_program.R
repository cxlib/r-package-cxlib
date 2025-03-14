#' Internal utility to execute a program as part of batch job actions
#' 
#' @param x Action
#' @param options Execution options
#' 
#' @returns A list of named entries representing the action results
#' 
#' @description
#' The routine takes two lists as input.
#' 
#' All paths are relative to the `work.area` specified in the `options` parameter.
#' 
#' The action `x` is defined as a nested list with the following named elements
#' 
#' \itemize{
#'   \item `type` is the action type and equal to `program`, case in-sensitive
#'   \item `id` identifies the action with an action ID
#'   \item `path` the program path
#'   \item `sha1` the program SHA-1 hash
#'   \item `log` a list that contains the log file path (`path`)
#' }
#' 
#' The action list of options `options` is defined as a nested list with the 
#' following named elements
#' 
#' \itemize{
#'   \item `job.id` identifies the action parent Job ID
#'   \item `work.area` represents the working directory for executing the program
#' }
#' 
#' The result returned is the action list with the additional named elements and updates.
#' 
#' \itemize{
#'   \item `log` from the action with the addition of the log file SHA-1 hash (`sha1`)
#'   \item `files.input` a list of input files in the work area at the start of execution
#'   \item `files.created` new files created in the work area during execution
#'   \item `files.updated` existing files updated in the work area during execution
#'   \item `files.deleted` files that existed and was deleted in the work area during execution
#' }
#' 
#' 
#' The entries of `files.input`, `files.created`, `files.updated` and `files.deleted` consist
#' of both the path `path` and SHA-1 hash `sha1`. For deleted files, the SHA-1 hash represents
#' the SHA-1 hash of the file before it was deleted. 
#' 
#'  
#' @keywords internal


.cxlib_batchjob_execd_program <- function( x, options ) {
  
  if ( missing(x) || is.null(x) || (length(x) == 0) || is.na(utils::head( x, n = 1)) || ! inherits( x, "list") )
    stop( "Action missing or invalid")
  
  if ( missing(options) || is.null(options) || any(is.na(options)) || ! inherits( options, "list") )
    stop( "Expected options missing")
  
  
  # -- action environment checks
  
  # - work area

  if ( ! "work.area" %in% names(options) || ! dir.exists( options[["work.area"]] ) )
    stop( "Work area is not defined or does not exist")

  work_area <- cxlib::cxlib_standardpath( unname( options[["work.area"]] ) )

  
  # - job id
  #   note: job ID is a UUID
  if ( ! "job.id" %in% names(options) || is.null(options[["job.id"]]) || any(is.na(options[["job.id"]])) || 
       ! inherits(options[["job.id"]], "character") || ! uuid::UUIDvalidate(options[["job.id"]]) )
    stop( "Job is not identifiable" )
  
  job_id <- options[["job.id"]]
  
  
  # -- integrity checks

  # - action id
  #   note: action id is in the format of a SHA-1 hash
  
  if ( ! "id" %in% names(x) ||
       ! grepl( "^[a-f0-9]{8}(-[a-f0-9]{8}){4}$", x[["id"]], ignore.case = TRUE, perl = TRUE) )
    stop( "Action ID is missing or in an invalid format" )
       

    
  # - R program
  
  if ( ! "type" %in% names(x) || 
       ( base::toupper(x[["type"]]) != "PROGRAM" ) ) 
    stop( "The submitted action is not a program action")
  

  if ( ! "path" %in% names(x) )
    stop( "The program path missing from program action")

  
  if ( ! base::toupper(tools::file_ext(x[["path"]])) %in% c( "R" )  )
    stop( "The program file extension not of a supported program type" )
  
  
  # - program file 
  
  pgm_file <- file.path( options[["work.area"]], x[["path"]], fsep = "/" )

  if ( ! file.exists( pgm_file ) )
    stop( "Program does not exist" )
  

  if ( ! "sha1" %in% names(x) ||
       ( digest::digest( pgm_file, algo = "sha1", file = TRUE ) != x[["sha1"]]  ) )
    stop( "Program could not be verified or has changed since action was defined" )
  
    
  # - log file
  
  if ( ! "log" %in% names(x) || 
       ! "path" %in% names(x[["log"]]) )
    stop( "Log path is not defined" )
  
  
  if ( ! base::dir.exists( file.path( options[["work.area"]], base::dirname( x[["log"]][["path"]] ), fsep = "/" ) ) )
    stop( "Log directory does not exist")

  
  
  # -- initiate action results
  
  exec_results <- append( x, 
                          list( "start" = NA, 
                                "end" = NA, 
                                "files.input" = list(), 
                                "files.created" = list(), 
                                "files.updated" = list(), 
                                "files.deleted" = list() ) )
  
  
  # -- pre-execution inventory 
  
  pre_inv <- sapply( list.files( work_area, recursive = TRUE, full.names = FALSE, include.dirs = FALSE ), function(y) {
    digest::digest( file.path( work_area, y, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  # - add inputs to execution results
  
  exec_results[["files.input"]] <- lapply( names(pre_inv), function(y) {
    list( "path" = cxlib::cxlib_standardpath( y ), 
          "sha1" = unname(pre_inv[y]) )
  })
  
  

  # -- batch execute program

  batch_args <- character(0)
  
  # - add R workspace flags
  batch_args <- append( batch_args, c( "--no-restore --no-save" ) )
  
  # - add program 
  batch_args <- append( batch_args, x[["path"]] )
  
  # - add log 
  batch_args <- append( batch_args, x[["log"]][["path"]] )
  

  
  # - start time
  exec_results[["start"]] <- format( as.POSIXct( base::Sys.time(), tz = "UTC"), format = "%Y%m%dT%H%M%S")
  
    
  # - run program
  rc <- try( callr::rcmd( "BATCH", 
                          cmdargs = batch_args, 
                          wd = work_area, 
                          echo = FALSE, 
                          show = FALSE, 
                          spinner = FALSE ),
             silent = TRUE )
  
  
  # - end time
  exec_results[["end"]] <- format( as.POSIXct( base::Sys.time(), tz = "UTC"), format = "%Y%m%dT%H%M%S")
  

  # - assess run  
  if ( inherits( rc, "try-error") || ! file.exists( file.path( work_area, exec_results[["log"]]["path"], fsep = "/" ) ) )
    stop( "Executing program ", x, " failed" )
  
  
  
  # -- register log reference with results  
  exec_results[["log"]][["sha1"]] <- digest::digest( file.path( work_area, exec_results[["log"]][["path"]], fsep = "/" ), algo = "sha1", file = TRUE )

  
  # -- post-execution inventory
  
  post_inv <- sapply( list.files( work_area, recursive = TRUE, full.names = FALSE, include.dirs = FALSE ), function(y) {
    digest::digest( file.path( work_area, y, fsep = "/"), algo = "sha1", file = TRUE )
  }, USE.NAMES = TRUE )
  
  
  
  # -- identify created files
  
  created <- post_inv[ ! names(post_inv) %in% names(pre_inv) ]
  
  exec_results[["files.created"]] <- lapply( sort(names(created)), function(z) {
    list( "path" = z, "sha1" = unname(created[z]) )
  } )
  
  
  
  # -- identify updated files
  
  updated_files <- unlist( lapply( base::intersect( names(pre_inv), names(post_inv) ), function(z) {
    
    if ( post_inv[ z ] != pre_inv[ z ] ) 
      return(z)
    else
      return(NULL)
    
  }), use.names = FALSE )  
  
  
  exec_results[["files.updated"]] <- lapply( sort(updated_files), function(z) {
    list( "path" = z, "sha1" = unname(post_inv[z]) )
  })
  
  
  
  # -- identify deleted files
  
  deleted <- pre_inv[ ! names(pre_inv) %in% names(post_inv) ]
  
  exec_results[["files.deleted"]] <- lapply( sort(names(deleted)), function(z) {
    list( "path" = z, "sha1" = unname(deleted[z]) )
  } )
  

  
  
  
  # -- update log
  
  
  # - log preamble
  
  log_preamble <- paste(rep_len("-", 65), collapse = "")
  
  # identify execution
  log_preamble <- append( log_preamble, 
                          c( exec_results[["id"]],
                             " ",
                             paste( "Job ID    :", job_id ),
                             paste( "Started   :", gsub( "(\\d{4})(\\d{2})(\\d{2})T(\\d{2})(\\d{2})(\\d{2})", "\\1-\\2-\\3 \\4:\\5:\\6", exec_results[["start"]]) ),
                             paste( "Completed :", gsub( "(\\d{4})(\\d{2})(\\d{2})T(\\d{2})(\\d{2})(\\d{2})", "\\1-\\2-\\3 \\4:\\5:\\6", exec_results[["end"]]) ),
                             base::rep_len( " ", 2 ) ) )
  
  # identify input files
  log_preamble <- append( log_preamble,
                          c( "Input files", 
                             paste(rep_len("-", 35), collapse = "") ) )
  
  
  for ( xfile in sort(names(pre_inv)) )
    log_preamble <- append( log_preamble, 
                            c( xfile, 
                               paste0( "(SHA-1: ", pre_inv[xfile], ")" ), 
                               " ") )
  
  
  log_preamble <- append( log_preamble, paste(rep_len("-", 65), collapse = "") )
  
  # -- end of log preamble
  
  
  
  # -- log results
  
  log_result <- c( paste(rep_len("-", 65), collapse = ""), 
                   "Program execution results" ) 
  
  # identify created files
  log_result <- append( log_result,
                        c( base::rep_len( " ", 2 ), 
                           "Created files", 
                           paste(rep_len("-", 35), collapse = "") ) )
  
  if ( length(created) == 0 )
    log_result <- append( log_result, "None" ) 
  
  for ( xfile in sort(names(created)) ) {
    
    # add file path reference
    log_result <- append( log_result, xfile )
    
    
    # add SHA-1 reference
    
    xfile_sha1 <- paste0( "(SHA-1: ", created[xfile], ")" )
    
    if ( xfile == exec_results[["log"]]["path"] )
      xfile_sha1 <- "(This log file)"
    
    log_result <- append( log_result, c( xfile_sha1, " " ) )
    
  }
  
  
  # identify updated files
  log_result <- append( log_result,
                        c( base::rep_len( " ", 2 ), 
                           "Updated files", 
                           paste(rep_len("-", 35), collapse = "") ) )
  
  if ( length(updated_files) == 0 )
    log_result <- append( log_result, "None" )
  
  
  for ( xfile in sort(names(updated_files)) ) {
    
    # add file path reference
    log_result <- append( log_result, xfile )
    
    
    # add SHA-1 reference
    
    xfile_sha1 <- paste0( "(SHA-1: ", updated_files[xfile], ")" )
    
    if ( xfile == exec_results[["log"]]["path"] )
      xfile_sha1 <- "(This log file)"
    
    log_result <- append( log_result, c( xfile_sha1, " " ) )
    
  }
  
  
  
  # identify deleted files
  log_result <- append( log_result,
                        c( base::rep_len( " ", 2 ), 
                           "Deleted files", 
                           paste(rep_len("-", 35), collapse = "") ) )
  
  if ( length(deleted) == 0 )
    log_result <- append( log_result, "None" )
  
  for ( xfile in sort(names(deleted)) )
    log_result <- append( log_result, 
                          c( xfile, 
                             paste0( "(SHA-1: ", deleted[xfile], ")" ), 
                             " ") )
  
  log_result <- append( log_result, c( " ", paste(rep_len("-", 65), collapse = "") ) )
  
  # -- end of log results
  
  
  # -- update log
  
  # - read in the current log
  log_lines <- base::readLines( con = file.path( work_area, exec_results[["log"]][["path"]], fsep = "/") , warn = FALSE )
  
  
  # - add preamble and results
  updated_log_lines <- c( paste( "#>", log_preamble, sep = "  "),
                          base::rep_len( " ", 3 ),
                          log_lines, 
                          base::rep_len( " ", 3 ),
                          paste( "#>", log_result, sep = "  " ) )
  
  
  # - re-write log and update SHA-1 for log in associated entries
  
  base::writeLines( updated_log_lines, 
                    con = file.path( work_area, exec_results[["log"]][["path"]], fsep = "/") )  
  
  exec_results[["log"]][["sha1"]] <- digest::digest( file.path( work_area, exec_results[["log"]][["path"]], fsep = "/" ), 
                                                     algo = "sha1", 
                                                     file = TRUE )
  
  # update log entries in audit details
  
  for ( xcat in c( "files.created", "files.updated") )
    if ( length(exec_results[[xcat]]) > 0 )
      for ( xentry in 1:length(exec_results[[xcat]]) ) 
        if ( unname(exec_results[[xcat]][[xentry]][["path"]]) == unname(exec_results[["log"]][["path"]]) )
          exec_results[[xcat]][[xentry]][["sha1"]] <- exec_results[["log"]][["sha1"]]
    
  
  
  
  
  return(invisible(exec_results))
}