#' Internal function representing a cx.batch.r action to execute an R program
#' in batch
#' 
#' @param x Definition of action
#' @param work.area Work area path
#' 
#' @returns A list of named entries representing the action results
#' 
#' @description
#' The `cx.batch.r` action, or the generalized `cx.batch` action when the 
#' action attribute `path` file extension equals `.R`, case insensitive, 
#' represents an action to execute an R program in batch.
#' 
#' The input `x` is a nested list in the form of
#' \preformatted{
#' list( "id" = <id>,
#'       "type" = <action type>,
#'       "attributes" = list( "path" = <path>, 
#'                            "path.sha" = <SHA-1 of file in path>,
#'                            "log" = <log file path>,
#'                            "logs" = <path to log parent directory>,
#'                            "log.fileext" = <log file extension>
#'                            ... ),
#'        "job.id" = <job ID> )
#' }
#' 
#' The `id` entry represntes the assigned action ID.
#' 
#' The entry `job.id` is the parent Job ID for the action. An action cannot be
#' defined independent of a job.  
#' 
#' The required `path` attribute is the relative path to the R program to
#' execute and `path.sha` is the corresponding SHA-1 of the file.
#' 
#' The `log`, `logs` and `log.fileext` attributes are optional defines the log
#' file path and name. If `log` attribute exists, it is assumed to define the 
#' destination of the program log, including log file name and file extension and
#' the `log` and `log.fileext` attributes are ignored.
#' 
#' The `logs` attribute specifies the log file parent directory. If `logs` is 
#' not defined as an attribute or corresponding configuration property (below),
#' the log output directory is the parent directory of the program. 
#' 
#' The `log.fileext` attribute can be used to specify the log file extension. 
#' The log file name is the program name but with the log file extension. If
#' `log.fileext` is not defined as an attribute or corresponding configuration 
#' property (below), the default log file extension is `Rout`.
#' 
#' The `logs` and `log.fileext` can also be defined by configuration properties 
#' in the specified order of precedence.
#' \itemize{
#'   \item `logs` correspond to the properties `CX.BATCH.R.LOGS` and `CX.BATCH.LOGS` 
#'   \item `log.fileext` correspond to the properties `CX.BATCH.R.LOG.FILEEXT` 
#'          and `CX.BATCH.LOG.FILEEXT` 
#' }
#' 
#' If either of `logs` and `log.fileext` are specified as attributes and as 
#' configuration properties, the respective attribute values take precedence.
#' 
#' 
#' The returned list includes input `x` and action `results` as a nested list.
#' 
#' \preformatted{
#' list( ...,
#'       "results" = list( "status" = <action status>, 
#'                         "start" = <action start time>,
#'                         "end" = <action completion time>,
#'                         "files.input" = list( list( "path" = <file path>,
#'                                                     "sha" = <SHA-1 of file> ), 
#'                                                     ... ),
#'                         "files.created" = list( list( "path" = <file path>,
#'                                                       "sha" = <SHA-1 of file> ), 
#'                                                 ... ),
#'                         "files.updated" = list( list( "path" = <file path>,
#'                                                       "sha" = <SHA-1 of file> ), 
#'                                                 ... ),
#'                         "files.deleted" = list( list( "path" = <file path>,
#'                                                       "sha" = <SHA-1 of file> ), 
#'                                                 ... ) )
#' }
#' 
#' If the `log` attribute was not included specified as part of `x`, the `log`
#' entry is added to top-level of the returned list. 
#' 
#' The returned `log` entry is in the form of 
#' \preformatted{
#' list( ..., 
#'       "log" = list( "path" = <log file path>
#'                     "sha" = <SHA-1 of log file> ), 
#'       "results" = ... )
#' 
#' }
#' 
#' 
#' 
#' 
#' 
#' @keywords internal

.cxaction_batchr <- function( x, work.area = base::getwd() ) {
  
  
  # -- configuration options
  cfg <- cxapp::cxapp_config()
  

  # -- verify inputs
  
  if ( missing(x) || ! inherits( x, "list") || 
       is.null(base::names(x)) ||
       ! all(c( "id", "type", "attributes", "job.id" ) %in% base::tolower(base::names(x)) ) )
    stop( "The action is missing one or more required standard definition properties" )
  
  
  if ( is.null(work.area) || ! inherits( work.area, "character" ) || (length(work.area) != 1) || (base::trimws(work.area) == "") ||
       ! dir.exists( work.area ) )
    stop( "The work area directory is missing, invalid or does not exist" )
  
  
  
  # - standardize to lower case attribute names
  base::names(x[["attributes"]]) <- base::tolower(base::names(x[["attributes"]]))
  
  
  if ( (length(x[["type"]]) != 1) || 
       ! base::tolower(x[["type"]]) %in% c( "cx.batch.r", "cx.batch" ) )  
    stop( "The action is not supported" )
  
  if ( ! all( c( "path", "path.sha" ) %in% base::names(x[["attributes"]]) ) )
    stop( "One or more required action attributes missing")
  
    
  
  # -- init action result 
  
  # - initiate action results
  
  action_results <- x
  
  # - add results
  action_results[["results"]] <- list( "status" = "init",
                                       "start" = NA, 
                                       "end" = NA, 
                                       "log" = list( "path" = NA,
                                                     "sha" = NA ),
                                       "files.input" = list(), 
                                       "files.created" = list(), 
                                       "files.updated" = list(), 
                                       "files.deleted" = list(), 
                                       "messages" = character(0) ) 
  

  # --  root of program work area
  root_wa <- cxlib::cxlib_standardpath( work.area )

  
  
  # -- pre-inventory
  pre_inv <- sapply( cxlib::cxlib_standardpath( list.files( root_wa, recursive = TRUE, include.dirs = FALSE, full.names = FALSE) ), function(wafile) {
    digest::digest( file.path( root_wa, wafile, fsep = "/"), algo = "sha1", file = TRUE ) 
  })
  
  
  # - add file inventory to action results
  
  # noting input links for later processing
  input_links <- character(0)
  
  action_results[["results"]][["files.input"]] <- lapply( base::names(pre_inv), function(wafile) {
    
    wafile_info <- list( "path" = wafile, 
                         "sha" = pre_inv[[wafile]] )
    
    
    # - check for linked files
    #   note: a linked file is a "placeholder" for a file with content "cx.link:sha:<SHA-1>:md5:<MD-5" where SHA-1 and MD-5 are for the original file
    #   note: files.input represents the files at source and not the link
    #   note: the formatted string should result in lightweight file no more than 300 bytes, most likely less than 100 bytes on most systems
    #   note: 300 byte threshold may change if the file is encoded with more information
    wafile_size <- try( base::file.info( file.path( root_wa, wafile, fsep = "/") )[, "size"], silent = TRUE )
    
    if ( ! inherits( wafile_size, "try-error") &&
         ( wafile_size <= 300 ) ) {
      
      lnk_info <- try( base::readChar( file.path( root_wa, wafile, fsep = "/"), wafile_size, useBytes = TRUE), silent = TRUE )
      
      if ( ! inherits( lnk_info, "try-error") &&
           grepl( "^cx.link\\:(sha|md5)\\:.*", lnk_info, ignore.case = TRUE ) )  {
        
        lnk_lst <- base::unlist(base::strsplit( lnk_info, ":", fixed = TRUE) )

        if ( ! is.na(match( "sha", base::tolower(lnk_lst))) ) {

          # input files record
          wafile_info[["sha"]] <- lnk_lst[ match( "sha", base::tolower(lnk_lst)) + 1 ]

          # noted link 
          input_links[ wafile ] <- base::unname(wafile_info[["sha"]])
          
        }
          
      }  #  if-statement identifying lnk_info as a cx.link
      
    }  # if-statement to successful file.info() and file size bytes within trigger boundary
    
    
    # - return work area file path and SHA-1
    wafile_info
    
  })
  
  

  # -- verify program action  
  
  # - program file exists
  if ( ! inherits( x[["attributes"]][["path"]], "character" ) || 
       ( length(x[["attributes"]][["path"]]) != 1 ) ||
       ( base::trimws(x[["attributes"]][["path"]]) == "" ) ||
       ! file.exists( file.path( root_wa, x[["attributes"]][["path"]], fsep = "/" ) ) ) {
    
    action_results[["results"]][["status"]] <- "error"
    action_results[["results"]][["messages"]] <- "Program path does not exist in compute work area" 
    
    return(invisible(action_results))
  }
  
  
  xpath_pgm <- x[["attributes"]][["path"]]

    
  # - program file is an R program when using more generalized batch action cx.batch
  
  if ( ( base::tolower(x[["type"]]) == "cx.batch" ) &&
       ( base::toupper( tools::file_ext( xpath_pgm ) ) != "R" ) ) {
    
    action_results[["results"]][["status"]] <- "error"
    action_results[["results"]][["messages"]] <- "Expecting a program with file extension .R for action cx.batch" 
    
    return(invisible(action_results))
  }
  
  
  # - program file integrity
  
  if ( ! inherits( x[["attributes"]][["path.sha"]], "character" ) || 
       ( length(x[["attributes"]][["path.sha"]]) != 1 ) ||
       ( base::trimws(x[["attributes"]][["path.sha"]]) == "" ) ||
       ( x[["attributes"]][["path.sha"]] != pre_inv[[ xpath_pgm ]] ) ) {
    
    action_results[["results"]][["status"]] <- "error"
    action_results[["results"]][["messages"]] <- "Program file SHA-1 integrity check failure" 
    
    return(invisible(action_results))
  }
    
  
  
  # -- setup to execute program
  
  
  # - log file
  
  # note: initialize with default log
  # note: use program parent directory if config not defined
  # note: use program file name with either extension .Rout or defined by config
  xpath_log <- paste( cfg$option( c("cx.batch.r.logs", "cx.batch.logs" ), unset = base::dirname(xpath_pgm) ),
                      paste0( tools::file_path_sans_ext(base::basename(xpath_pgm)), 
                              ".", 
                              cfg$option( c("cx.batch.r.log.fileext", "cx.batch.log.fileext" ), unset = "Rout" )),
                      sep = "/" )

  # note: log overrides
  if ( "log" %in% base::names(x[["attributes"]]) ) 
    xpath_log <- file.path( root_wa, x[["attributes"]][["log"]], fsep = "/" )
    
  
  # note: using relative paths in action results
  action_results[["results"]][["log"]] <- list( "path" = xpath_log,
                                                "sha" = NA )
  

  # - command sequence

  batch_args <- character(0)
  
  # - add R workspace flags
  batch_args <- append( batch_args, c( "--no-restore --no-save" ) )
  
  # - add program 
  batch_args <- append( batch_args, xpath_pgm )
  
  # - add log 
  batch_args <- append( batch_args, xpath_log )
  
    
  
  # -- execute program

  # - start time
  action_results[["results"]][["start"]] <- format( as.POSIXct( base::Sys.time(), tz = "UTC"), format = "%Y%m%dT%H%M%S")
  
  
  # - run program
  rc <- try( callr::rcmd( "BATCH", 
                          cmdargs = batch_args, 
                          wd = root_wa, 
                          echo = FALSE, 
                          show = FALSE, 
                          spinner = FALSE ),
             silent = FALSE )
  
  
  # - end time
  action_results[["results"]][["end"]] <- format( as.POSIXct( base::Sys.time(), tz = "UTC"), format = "%Y%m%dT%H%M%S")
  
  
  # - set default status
  action_results[["results"]][["status"]] <- "completed"
  
  
  # - post execution assessment
  if ( inherits( rc, "try-error") || ! file.exists( file.path( root_wa, xpath_log, fsep = "/" ) ) ) {
    action_results[["results"]][["status"]] <- "error"
    action_results[["results"]][["messages"]] <- "Executing program failed"
    
    if ( ! file.exists( file.path( root_wa, xpath_log, fsep = "/" ) ) )
      action_results[["results"]][["messages"]] <- append( action_results[["results"]][["messages"]], 
                                                           "Log file does not exist" )
  }
  

  # -- post inventory
  post_inv <- sapply( cxlib::cxlib_standardpath( list.files( root_wa, recursive = TRUE, include.dirs = FALSE, full.names = FALSE) ), function(wafile) {
    digest::digest( file.path( root_wa, wafile, fsep = "/"), algo = "sha1", file = TRUE ) 
  })
  

  
  # - identify created files
  
  created <- post_inv[ ! base::names(post_inv) %in% base::names(pre_inv) ]
  
  action_results[["results"]][["files.created"]] <- lapply( base::sort(base::names(created)), function(z) {
    list( "path" = z, 
          "sha" = base::unname(created[z]) )
  })
  
  
  
  # - identify updated files
  
  updated_files <- base::unlist( lapply( base::intersect( base::names(pre_inv), base::names(post_inv) ), function(z) {
    
    if ( post_inv[ z ] != pre_inv[ z ] ) 
      return(z)
    else
      return(NULL)
    
  }), use.names = FALSE )  
  
  
  if ( length(updated_files) > 0 ) {
   
    # register update files in results 
    action_results[["results"]][["files.updated"]] <- lapply( base::sort(updated_files), function(z) {
      list( "path" = z, 
            "sha" = base::unname(post_inv[z]) )
    })

    
    # manage copied links
    if ( any( updated_files %in% input_links ) ) {
      
      action_results[["results"]][["status"]] <- "error"

      for ( xfile in updated_files[ updated_files %in% input_links ] ) 
        action_results[["results"]][["messages"]] <- append( action_results[["results"]][["messages"]], 
                                                             paste( "File", xfile, "is a @cx.link and use as a source for file updates is not supported" ) ) 
    }

  }
    

  
  # - identify deleted files
  
  # note: create a pre-inventory lookup with links resolved 
  pre_inv_resolvlinks <- base::unlist(lapply( action_results[["results"]][["files.input"]], function(y) {
   base::unname(y[["sha"]])
  }), use.names = FALSE )
  
  base::names(pre_inv_resolvlinks) <- base::unlist(lapply( action_results[["results"]][["files.input"]], function(y) {
    base::unname(y[["path"]])
  }), use.names = FALSE )
  
  

  deleted <- pre_inv[ ! base::names(pre_inv) %in% base::names(post_inv) ]
  
  action_results[["results"]][["files.deleted"]] <- lapply( base::sort(base::names(deleted)), function(z) {
    list( "path" = z, 
          "sha" = base::unname(pre_inv_resolvlinks[z]) )
  } )

  

  # -- post process log

  if ( ! file.exists( file.path( root_wa, xpath_log, fsep = "/" ) ) )  
    return(invisible(action_results))
  

    
  # - import log  
  
  log_lines <- base::readLines( file.path( root_wa, action_results[["results"]][["log"]][["path"]], fsep = "/" ), warn = FALSE )
  
  
  # - check for execution errors
  
  if ( any( grepl( "^error\\:", log_lines, ignore.case = TRUE ) ) ) {
    
    action_results[["results"]][["status"]] <- "fail"
    action_results[["results"]][["messages"]] <- append( action_results[["results"]][["messages"]], 
                                                         "One or more execution errors identified in the log" ) 
  }
  
  
  if ( any( grepl( "^warning\\:", log_lines, ignore.case = TRUE ) ) ) {
    action_results[["results"]][["status"]] <- "fail"
    action_results[["results"]][["messages"]] <- append( action_results[["results"]][["messages"]], 
                                                         "One or more execution warnings identified in the log" ) 
  }
  

  
  # - standardize no messages
    
  if ( length(action_results[["results"]][["messages"]]) == 0 )
    action_results[["results"]][["messages"]] <- "None"
  
  
  # - log preamble
  
  log_preamble <- paste(rep_len("-", 65), collapse = "")
  
  # identify execution
  log_preamble <- append( log_preamble, 
                          c( action_results[["id"]],
                             " ",
                             paste( "Job ID    :", action_results[["job.id"]] ),
                             paste( "Started   :", gsub( "(\\d{4})(\\d{2})(\\d{2})T(\\d{2})(\\d{2})(\\d{2})", "\\1-\\2-\\3 \\4:\\5:\\6", action_results[["results"]][["start"]]) ),
                             paste( "Completed :", gsub( "(\\d{4})(\\d{2})(\\d{2})T(\\d{2})(\\d{2})(\\d{2})", "\\1-\\2-\\3 \\4:\\5:\\6", action_results[["results"]][["end"]]) ),
                             base::rep_len( " ", 2 ) ) )
  
  
  # add execution result
  log_preamble <- append( log_preamble, 
                          c( paste( "Result    :", action_results[["results"]][["status"]]), 
                             " ", 
                             "Execution messages", 
                             paste(rep_len("-", 35), collapse = "") ) )
  
  for ( xmsg in action_results[["results"]][["messages"]] )
    log_preamble <- append( log_preamble, xmsg )
  
  log_preamble <- append( log_preamble, base::rep_len( " ", 2 ) )
  
  
  
  # - trace input files

  log_preamble <- append( log_preamble,
                          c( "Input files", 
                             paste(rep_len("-", 35), collapse = "") ) )
  
  
  if ( length(action_results[["results"]][["files.input"]]) == 0 ) {
    
    # no records
    log_preamble <- append( log_preamble, "None" ) 
    
  } else {
    
    # record each entry
    for ( xentry in 1:length(action_results[["results"]][["files.input"]]) ) 
      log_preamble <- append( log_preamble, 
                              c( action_results[["results"]][["files.input"]][[xentry]][["path"]], 
                                 paste0( "(SHA-1: ", action_results[["results"]][["files.input"]][[xentry]][["sha"]], ")" ), 
                                 "") )
    

  } #  end else in if-statement for files.input
  

  # - end of preamble
  log_preamble <- append( log_preamble, paste(rep_len("-", 65), collapse = "") )
  


  # - execution trace
  
  log_trace <- c( paste(rep_len("-", 65), collapse = ""), 
                   "Program execution results" ) 
  
  
  # - trace created files
  log_trace <- append( log_trace,
                        c( base::rep_len( " ", 2 ), 
                           "Created files", 
                           paste(rep_len("-", 35), collapse = "") ) )
  
  
  if ( length(action_results[["results"]][["files.created"]]) == 0 ) {
    
    # no records
    log_trace <- append( log_trace, "None" ) 
    
  } else {
    
    # record each entry
    # note: if log file ... too early to derive SHA-1 
    for ( xentry in 1:length(action_results[["results"]][["files.created"]]) ) 
      log_trace <- append( log_trace, 
                           c( action_results[["results"]][["files.created"]][[xentry]][["path"]], 
                              ifelse( action_results[["results"]][["files.created"]][[xentry]][["path"]] == action_results[["results"]][["log"]][["path"]], 
                                      "(This log file)", 
                                      paste0( "(SHA-1: ", action_results[["results"]][["files.created"]][[xentry]][["sha"]], ")" ) ), 
                              "" ) )

  } #  end else in if-statement for files.created
  
  

  # - trace updated files
  
  log_trace <- append( log_trace,
                       c( base::rep_len( " ", 2 ), 
                          "Updated files", 
                          paste(rep_len("-", 35), collapse = "") ) )
  
  
  if ( length(action_results[["results"]][["files.updated"]]) == 0 ) {
    
    # no records
    log_trace <- append( log_trace, "None" ) 
    
  } else {
    
    # record each entry
    # note: if log file ... too early to derive SHA-1 
    for ( xentry in 1:length(action_results[["results"]][["files.updated"]]) ) 
      log_trace <- append( log_trace, 
                           c( action_results[["results"]][["files.updated"]][[xentry]][["path"]], 
                              ifelse( action_results[["results"]][["files.updated"]][[xentry]][["path"]] == action_results[["results"]][["log"]][["path"]], 
                                      "(This log file)", 
                                      paste0( "(SHA-1: ", action_results[["results"]][["files.updated"]][[xentry]][["sha"]], ")" ) ),
                              "") )
    
  } #  end else in if-statement for files.updated
  

  
  # - trace deleted files
  
  log_trace <- append( log_trace,
                       c( base::rep_len( " ", 2 ), 
                          "Deleted files", 
                          paste(rep_len("-", 35), collapse = "") ) )
  
  
  if ( length(action_results[["results"]][["files.deleted"]]) == 0 ) {
    
    # no records
    log_trace <- append( log_trace, "None" ) 
    
  } else {
    
    # record each entry
    # note: if log file ... too early to derive SHA-1 
    for ( xentry in 1:length(action_results[["results"]][["files.deleted"]]) ) 
      log_trace <- append( log_trace, 
                           c( action_results[["results"]][["files.deleted"]][[xentry]][["path"]], 
                              paste0( "(SHA-1: ", action_results[["results"]][["files.deleted"]][[xentry]][["sha"]], ")" ),
                              "" ) ) 
    
  } #  end else in if-statement for files.deleted

  
  # - end of results trace
  log_trace <- append( log_trace, c( "", paste(rep_len("-", 65), collapse = "") ) )
  
  

  
  # - update log with preamble and trace
  
  log_updt <- c( paste( "#>", log_preamble, sep = "  "),
                 base::rep_len( " ", 3 ),
                 log_lines, 
                 base::rep_len( " ", 3 ),
                 paste( "#>", log_trace, sep = "  " ) )
  

  # - rewrite log
  base::writeLines( log_updt, 
                    con = file.path( file.path( root_wa, action_results[["results"]][["log"]][["path"]], fsep = "/" ), fsep = "/") )  
  
  action_results[["results"]][["log"]][["sha"]] <- digest::digest( file.path( root_wa, action_results[["results"]][["log"]][["path"]], fsep = "/" ), 
                                                                   algo = "sha1", 
                                                                   file = TRUE )
    
  
  # -- update log entries in audit details
  #    note: stay safe and traverse all entries
  
  for ( xcat in c( "files.created", "files.updated") )
    if ( length(action_results[["results"]][[xcat]]) > 0 )
      for ( xentry in 1:length(action_results[["results"]][[xcat]]) ) 
        if ( action_results[["results"]][[xcat]][[xentry]][["path"]] == action_results[["results"]][["log"]][["path"]] )
          action_results[["results"]][[xcat]][[xentry]][["sha"]] <- action_results[["results"]][["log"]][["sha"]]
  
  

  # -- return results 
  return(invisible(action_results))
}