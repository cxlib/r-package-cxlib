#' Utility class representing a Job
#' 
#' @field .attr Internal class attribute storage
#' 
#' @method new new
#' @method initialize initialize
#' @method actions actions
#' @method add add
#' @method remove remove
#' @method show show
#' @method toJSON toJSON
#' @method fromJSON fromJSON
#' 
#' 
#' @description
#' A \emph{job} is an ordered collection of tasks and processing actions, such 
#' as executing a program. The tasks and processing actions within a job are
#' expected to be processed sequentially in the registered sequence.
#' 
#' The `add` method is used to amend the job definition by defining actions 
#' and staging programs and inputs. The action is identified by the action
#' `type` and associated action attributes. Attributes can be defined by
#' named arguments or as a list of named elements. The first argument to the 
#' `add` method is always the action `type`.
#' 
#' The action is appended to the end of the registered job actions or `after` a 
#' specified action. An action can be referred to by the action position or the
#' action identifier.
#' 
#' The action `type` equal to `cx.batch` or `cx.batch.r` registers an action to
#' execute one or more R programs in batch mode. The action requires a vector
#' `paths` that specifies one or more paths to the respective R programs. If 
#' more than one path is specified, the action is replicated for each path. 
#' 
#' All paths in a job definition is assumed relative to the current working 
#' directory, i.e. using the current working directory as representing the 
#' root of a repository.
#'  
#' The method `actions` will return the list of actions.
#' 
#' The `drop` method removes a specified action `x` from the list of actions 
#' where `x` represents the position of the action or the action identifier. If
#' position `x` is larger than the number of actions, the last action is 
#' dropped.
#' 
#' The methods `.toJSON` and `.fromJSON` are internal methods to convert job 
#' definition to and from JSON format, respectively.
#' 
#' 
#' 
#' @exportClass cxlib_job
#' @export cxlib_job



cxlib_job <- methods::setRefClass( "cxlib_job", 
                                   fields = list( ".attr" = "list") )


cxlib_job$methods( "new" = function() {
  "New job definition"
  # -- stub to satisfy R CMD check
})


cxlib_job$methods( "initialize" = function() {
  "Initialize job definition"

    
  # -- initialize defaults
  
  .self$.attr <- list( "id" = cxlib:::.cxlib_referenceid( type = "job" ),
                       "actions" = list(),
                       "mode.silent" = TRUE )

  
  # -- configuration options
  
  cfg <- cxlib::cxlib_config()
  
  # - debug options
  .self$.attr[["mode.silent"]] <- ! cfg$option( "mode.debug", unset = FALSE )
  
  
  # # - default options
  # for ( xitem in c( "logs", "log.fileext" ) )
  #   if ( ! is.na( cfg$option( paste0( "CXLIB.", base::toupper(xitem)), unset = NA ) ) )
  #     .self$.attr[["options"]][[ xitem ]] <- cfg$option( paste0( "CXLIB.", base::toupper(xitem)), unset = NA )
  # 
  # 
  # 
  # # - root path 
  # #   note: root path is the parent directory for the job control directories
  # 
  # if ( ! is.na(cfg$option( "CXLIB.PATH", unset = NA )) &&
  #      ( (length(cfg$option( "CXLIB.PATH", unset = NA )) != 1 ) ||
  #        ! dir.exists( cfg$option( "CXLIB.PATH", unset = NA ) ) ) )
  #   stop( "The directory defined by CXLIB.PATH configuration property is invalid or does not exist" )
  # 
  # 
  # root_ctlpath <- cxlib::cxlib_standardpath( cfg$option( "CXLIB.PATH", unset = file.path( base::tempdir(), ".cxlib", fsep = "/") ) )
  # 
  # 
  # 
  # 
  # # -- manage specified identifier  
  # 
  # # - specified
  # 
  # if ( ! missing(x) ) {
  #   
  #   if ( is.null(x) || ! inherits( x, "character" ) || (length(x) != 1) || (base::trimws(x) == "") ||
  #        ! uuid::UUIDvalidate(base::trimws(x)) )
  #     stop( "The specified ID is in an invalid format" )
  # 
  #   .self$.attr[["id"]] <- base::trimws(x)
  #   
  # 
  #   if ( ! create && ! dir.exists( file.path( root_ctlpath, .self$.attr[["id"]], fsep =  ) ) )
  #     stop( "The job directory does not exist" )
  #   
  # }
  #   
  # 
  #   
  # # -- job directory areas and standard files
  #   
  # # - job control directory
  # .self$.attr[["paths"]]["job.control"] <- file.path( root_ctlpath, .self$.attr[["id"]], fsep = "/" )
  # 
  # 
  # # - job work area
  # #   note: the pattern .../.job./<job id>/.job./.work/... is used to identify system processes associated with a specific job
  # .self$.attr[["paths"]]["work.area"] <- file.path( cxlib::cxlib_standardpath( cfg$option( "CXLIB.WORK", 
  #                                                                                          unset = file.path( base::tempdir(), ".cxlib-wrk", fsep = "/") ) ), 
  #                                                   ".job.", .self$.attr[["id"]], ".job.", 
  #                                                   ".work", 
  #                                                   fsep = "/" )
  # 
  # # -- standard job control files
  # 
  # # - standard job definition file
  # .self$.attr[["paths"]]["job.definition"] <- file.path( .self$.attr[["paths"]]["job.control"], "job.json", fsep = "/" )
  # 
  # # - standard job definition lock file
  # #   note: if file exists, job is locked for editing
  # .self$.attr[["paths"]]["signal.editlock"] <- file.path( .self$.attr[["paths"]]["job.control"], "job.lck", fsep = "/" )
  # 
  # # - standard job processing - stop job
  # #   note: if file exists, job is/should stop processing with return of results
  # .self$.attr[["paths"]]["signal.stop"] <- file.path( .self$.attr[["paths"]]["job.control"], "stop.lck", fsep = "/" )
  # 
  # # - standard job processing - terminate job
  # #   note: if file exists, job is/should stop processing with no return of results
  # .self$.attr[["paths"]]["signal.terminate"] <- file.path( .self$.attr[["paths"]]["job.control"], "terminate.lck", fsep = "/" )
  
  
})





cxlib_job$methods( "actions" = function() {
  "List of actions and tasks defined in the job definition"


  return(invisible( base::unname(.self$.attr[["actions"]]) ))  
})


cxlib_job$methods( "add" = function( type, ..., after = 1000000L ) {
  "Add action or task to job definition"

    
  if ( missing(type) || ! inherits( type, "character" ) || (length(type) != 1) || (base::trimws(type) == "") )
    stop( "The specified action type is missing or invalid" )

  if ( ! grepl( "^[a-z0-9][a-z0-9\\.]{0,98}[a-z0-9]$", base::trimws(type), ignore.case = TRUE, perl = TRUE) )
    stop( "The action type is in an invalid format" )
  
  

  # -- initiate action def
  
  action_def <- list( "id" = cxlib:::.cxlib_referenceid( type = "action" ), 
                      "type" = base::tolower(base::trimws(type)),
                      "attributes" = list() )
  
  
  # -- action attributes
  
  attr_lst <- list(...)
  
  # - attributes as a named list of entries
  if ( ( length(attr_lst) == 1 ) && inherits( attr_lst[[1]], "list" ) )
    attr_lst <- attr_lst[[1]]
  
  if ( ( length(attr_lst) > 0 ) && 
       ( is.null(base::names(attr_lst)) || any( base::trimws(base::names(attr_lst)) == "" ) ) )
    stop( "All action attributes should be named" )
  
  # - standardize on lower case names
  if ( length(attr_lst) > 0 )
    base::names(attr_lst) <- base::tolower(base::trimws(base::names(attr_lst)))
  

  # - add attributes to action def
  action_def[["attributes"]] <- attr_lst
  
  
  # -- insert action def

  # - position as next action
  pos <- length(.self$.attr[["actions"]]) + 1

  # - first position ... after = 0
  if ( inherits( after, c( "numeric", "integer" ) ) && ( after == 0 ) )
    pos <- 1

  # - position as in numeric  
  if ( inherits( after, c( "numeric", "integer" ) ) && ( as.integer(after) < length(.self$.attr[["actions"]]) ) )
    pos <- as.integer(after) + 1
  
  
  # - position as action reference
  if ( inherits( after, "character" ) ) {
    
    if ( (length(after) != 1) || ! base::tolower(base::trimws(after)) %in% base::names(.self$.attr[["actions"]]) )
      stop( "The action reference specifed as insert position is invalid" )
    
    pos <- match( base::tolower(base::trimws(after)), base::names(.self$.attr[["actions"]]) ) + 1
  } 
  
  # - position out of range
  if ( length(.self$.attr[["actions"]]) + 1 < pos )
    pos <- length(.self$.attr[["actions"]]) + 1
  
  
  # - insert first action
  if ( length(.self$.attr[["actions"]]) == 0 ) {
    
    .self$.attr[["actions"]][[1]] <- action_def
    
  } else {
    
    #   note: one or more action records exist

    # - insert as first action
    if ( pos == 1 ) 
      .self$.attr[["actions"]] <- do.call( c, list( list(action_def), .self$.attr[["actions"]] ) )
    
    # - insert as last record
    if ( length(.self$.attr[["actions"]]) < pos ) 
      .self$.attr[["actions"]][[pos]] <- action_def
    
    # - insert at position 
    if ( (pos > 1) && (pos < length(.self$.attr[["actions"]])) )
      .self$.attr[["actions"]] <- do.call( c, list( .self$.attr[["actions"]][ 1:(pos-1) ], 
                                                    list(action_def),
                                                    .self$.attr[["actions"]][ pos:length(.self$.attr[["actions"]]) ] ) )
    
  }
  

  # - re-apply action names
  base::names(.self$.attr[["actions"]]) <- lapply( .self$.attr[["actions"]], function(x) { x[["id"]] } )
    

  return(invisible( .self$actions() ))  
})



cxlib_job$methods( "drop" = function( x ) {
  "Drop an action or task from job definition"
  
  if ( missing(x) || ! inherits( x, c( "numeric", "integer", "character" ) ) || (length(x) != 1) ||
       ( inherits( x, "character" ) && (base::trimws(x) == "") ) )
    stop( "The specified position or reference for the action to drop is missing or invalid" )
  
  
  # -- determine position 
  pos <- NA

  # - numeric position
  #   note: if x is larger than the list of actions, last action is dropped
  if ( inherits( x, c( "numeric", "integer" ) ) ) 
    pos <- min( as.integer(x), length(.self$.attr[["actions"]]) )
    


  # - position by reference  
  if ( inherits( x, "character" ) ) {
    
    if ( ! base::tolower(base::trimws(x)) %in% base::names(.self$.attr[["actions"]]) )
      return(invisible(NULL))
    
    pos <- match( base::tolower(base::trimws(x)), base::names(.self$.attr[["actions"]]) )

  }
    
  
  if ( is.na(pos) )
    stop( "Could not identify action to drop" )
  
  
  # -- drop actions
  
  # - drop first action
  if ( pos == 1 )
    .self$.attr[["actions"]] <- .self$.attr[["actions"]][ 2:length(.self$.attr[["actions"]]) ]
  
  # - drop last action
  if ( pos == length(.self$.attr[["actions"]]) )
    .self$.attr[["actions"]] <- .self$.attr[["actions"]][ 1:( length(.self$.attr[["actions"]]) - 1 ) ]
  
  # - drop middle action
  if ( (pos > 1) && (pos < length(.self$.attr[["actions"]])) )
    .self$.attr[["actions"]] <- do.call( c, list( .self$.attr[["actions"]][ 1:(pos-1) ], 
                                                  .self$.attr[["actions"]][ (pos+1):length(.self$.attr[["actions"]]) ] ) )
  
  
  # - re-apply action names
  base::names(.self$.attr[["actions"]]) <- lapply( .self$.attr[["actions"]], function(x) { x[["id"]] } )
  
  
  return(invisible( .self$actions() ))  
})





cxlib_job$methods( "toJSON" = function() {
  "Export job definition in JSON format"
  
  # -- base details
  lst <- list( "schema" = "cx.job.definition", 
               "version" = "0.1",
               "job" = .self$.attr[ c( "id", "actions") ] )
  
  
  # -- tweak format
  lst[["job"]][["actions"]] <- base::unname(lst[["job"]][["actions"]])

  
  # -- convert to JSON
  
  def_json <- try( jsonlite::toJSON( lst, pretty = TRUE, auto_unbox = TRUE ), silent = .self$.attr[["mode.silent"]] )
  
  if ( inherits( def_json, "try-error" ) )
    stop( "Could not create the definition in JSON format" )
  
    
  return(invisible( def_json ))
})



cxlib_job$methods( "fromJSON" = function( x ) {
  "Import job definition in JSON format"
  
  lst <- try( jsonlite::fromJSON(x), silent = .self$.attr[["mode.silent"]] )
  
  if ( inherits( lst, "try-error" ) )
    stop( "Could not import job from JSON format")
  
  
  if ( ! "job" %in% base::names(lst) ||
       ! all( c( "id", "actions" ) %in% base::names(lst[["job"]]) ) )
    stop( "One or more required elements missing in job definition" )
  
  
  for ( xitem in c( "id", "actions") )
    .self$.attr[[xitem]] <- lst[["job"]][[xitem]]
  
  
  return(invisible(TRUE))
})



cxlib_job$methods( "show" = function() {
  "Print job definition"
  
  
  # -- header
  
  job_info <- c( "Job definition", 
                 paste0( "(", .self$.attr[["id"]], ")" ),
                 "", 
                 "Actions", 
                 paste( rep_len( "-", 60 ), collapse = "" ), 
                 "" )
  
  
  # -- no actions
  if ( length(.self$.attr[["actions"]]) == 0 )
    job_info <- append( job_info, "No actions registered" )
  

  # -- with actions
  if ( length(.self$.attr[["actions"]]) > 0 )
    for ( xint in 1:length(.self$.attr[["actions"]]) ) {
      
      act_info <- c( paste0( "[", as.character(xint), "]"), 
                     paste( "Action", .self$.attr[["actions"]][[xint]][["type"]]),
                     paste0( "(", .self$.attr[["actions"]][[xint]][["id"]],")"),
                     "", 
                     "Properties and Attributes", 
                     paste( rep_len( "-", 60 ), collapse = "" ) )
      
      
      # - action attributes
      act_attr <- character(0)
      
      if ( length(.self$.attr[["actions"]][[xint]][["attributes"]]) == 0 )
        act_attr( "No properties or attributes configured" )
      
      
      if ( length(.self$.attr[["actions"]][[xint]][["attributes"]]) > 0 ) {
        
        column_width <- max( base::nchar(base::names(.self$.attr[["actions"]][[xint]][["attributes"]])) ) + 5
        
        act_attr <- base::unlist( lapply( base::names(.self$.attr[["actions"]][[xint]][["attributes"]]), 
                                          function(x) {   
                                            paste( x, paste( rep_len( " ", column_width - base::nchar(x) ), collapse = ""), .self$.attr[["actions"]][[xint]][["attributes"]][[x]] )
                                          } ) )
      }
      

      act_info <- c( act_info, act_attr, "", "" )      
      
      job_info <- append( job_info, act_info)
    }
  
  
  # -- display job
  cat( c( "", job_info, "" ), sep = "\n")
  
  
})






