#' Utility class representing a Job
#' 
#' @field .attr Internal class attribute storage
#' 
#' @method new new
#' @method initialize initialize
#' @method actions actions
#' @method add add
#' @method drop drop
#' @method show show
#' @method toJSON toJSON
#' @method fromJSON fromJSON
#' @method hash hash
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
#' The `hash` method derives a unique SHA-1 digest based on the job definition.
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
  .self$.attr[["mode.silent"]] <- ! base::tolower(cfg$option( "mode.debug", unset = "disable" )) %in% c( "enable", "enabled") 
  

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
  action_def[["attributes"]] <- as.list(attr_lst)
  
  
  # -- insert action def

  # - initialize position as next action
  after_pos <- length(.self$.attr[["actions"]])
  
  
  # - first position ... after = 0
  if ( inherits( after, c( "numeric", "integer" ) ) && ( after == 0 ) )
    after_pos <- 0

  # - position as in numeric  
  if ( inherits( after, c( "numeric", "integer" ) ) && ( as.integer(after) < length(.self$.attr[["actions"]]) ) )
    after_pos <- as.integer(after)
  
  
  # - position as action reference
  if ( inherits( after, "character" ) ) {
    
    if ( (length(after) != 1) || ! base::tolower(base::trimws(after)) %in% base::names(.self$.attr[["actions"]]) )
      stop( "The action reference specifed as insert position is invalid" )
    
    after_pos <- match( base::tolower(base::trimws(after)), base::names(.self$.attr[["actions"]]) )
  } 
  
  

  # - insert first action
  if ( length(.self$.attr[["actions"]]) == 0 ) {
    
    .self$.attr[["actions"]][[1]] <- action_def
    
  } else {
    
    #   note: one or more action records exist
    #   note: sequence of insert at position and last record is key to manage if-condition


    # - insert as first action
    if ( after_pos == 0 ) 
      .self$.attr[["actions"]] <- do.call( c, list( list(action_def), .self$.attr[["actions"]] ) )

    
    # - insert at position 
    if ( (after_pos > 0) && (after_pos < length(.self$.attr[["actions"]])) ) 
      .self$.attr[["actions"]] <- do.call( c, list( .self$.attr[["actions"]][ 1:after_pos ], 
                                                    list(action_def),
                                                    .self$.attr[["actions"]][ (after_pos + 1):length(.self$.attr[["actions"]]) ] ) )

    
    # - insert as last record
    if ( length(.self$.attr[["actions"]]) <= after_pos ) 
      .self$.attr[["actions"]][[ length(.self$.attr[["actions"]]) + 1 ]] <- action_def

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
  
  
  # -- futility
  if ( length(.self$.attr[["actions"]]) == 0 )
    return(invisible( .self$actions() ))
  
  
  # -- determine position 
  pos <- NA

  
  # - numeric position
  #   note: if x is larger than the list of actions, last action is dropped
  if ( inherits( x, c( "numeric", "integer" ) ) ) {

    if ( as.integer(x) < 1 )
      stop( "Position index invalid" )
    
    pos <- min( as.integer(x), length(.self$.attr[["actions"]]) )     
  } 



  # - position by reference  
  if ( inherits( x, "character" ) ) {
    
    if ( ! base::tolower(base::trimws(x)) %in% base::names(.self$.attr[["actions"]]) )
      return(invisible(NULL))
    
    pos <- match( base::tolower(base::trimws(x)), base::names(.self$.attr[["actions"]]) )

  }
    
  
  if ( is.na(pos) )
    stop( "Could not identify action to drop" )
  
  
  # -- drop actions
  #    note: the drop sequence is important so that dropping a record does not make the next
  #          condition true
  
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

  # - attributes
  #   note: moved from a named list of attribute values to list of attribute elements name and value
  for ( xaction in base::names(lst[["job"]][["actions"]]) ) {
    
    lst_attrs <- lst[["job"]][["actions"]][[xaction]][["attributes"]]
    
    mtx_attrs <- lapply( base::names(lst_attrs), function(x) {
      list( "name" = x, 
            "value" = lst_attrs[[x]] )
    })
    
    lst[["job"]][["actions"]][[xaction]][["attributes"]] <- mtx_attrs
    
  }
  

  # - action references
  lst[["job"]][["actions"]] <- base::unname(lst[["job"]][["actions"]])
  
  
  # -- convert to JSON
  
  def_json <- try( jsonlite::toJSON( lst, pretty = TRUE, auto_unbox = TRUE ), silent = .self$.attr[["mode.silent"]] )
  
  if ( inherits( def_json, "try-error" ) )
    stop( "Could not create the definition in JSON format" )
  
    
  return(invisible( def_json ))
})



cxlib_job$methods( "fromJSON" = function( x ) {
  "Import job definition in JSON format"
  
  lst <- try( jsonlite::fromJSON( x, simplifyVector = FALSE ), silent = .self$.attr[["mode.silent"]] )
  
  if ( inherits( lst, "try-error" ) )
    stop( "Could not import job from JSON format")
  
  
  if ( ! "schema" %in% base::names(lst) || ! "cx.job.definition" %in% base::tolower(as.character(lst[["schema"]])) )
    stop( "Missing or invalid schema reference")

  
  if ( ! "version" %in% base::names(lst) || ! base::tolower(as.character(lst[["version"]])) %in% c( "0.1") )
    stop( "Schema version missing or not supported" )
  
  
  if ( ! "job" %in% base::names(lst) ||
       ! all( c( "id", "actions" ) %in% base::names(lst[["job"]]) ) )
    stop( "One or more required elements missing in job definition" )
  
  
  # -- tweak import
  
  lst_def <- lst[["job"]]
  
  # - restore actions as named elements
  base::names(lst_def[["actions"]]) <- base::unlist(lapply( lst_def[["actions"]], function(x) { x[["id"]] } ))


  for ( xaction in base::names(lst_def[["actions"]]) ) {
    
    act_attrs <- list()
    
    for ( xattr in lst_def[["actions"]][[xaction]][["attributes"]] ) 
      act_attrs[[ xattr[["name"]] ]] <- xattr[["value"]]

    lst_def[["actions"]][[xaction]][["attributes"]] <- act_attrs
  }


  for ( xitem in c( "id", "actions") )
    .self$.attr[[xitem]] <- lst_def[[xitem]]
  
  
  return(invisible(TRUE))
})



cxlib_job$methods( "hash" = function() {
  "Derive hash value for job"
  
  # -- unpack internal attributes
  lst <- base::unlist( .self$.attr, use.names = TRUE )
  base::names(lst) <- base::tolower(base::names(lst))
  
  
  # -- generate one string per entry in format <key>=<value>
  lst_kv <- base::unlist( lapply( base::names(lst), function(x) {
    paste0( x, "=", lst[[x]] )
  }), use.names = FALSE )
  

  return(digest::digest( base::sort(lst_kv), algo = "sha1", file = FALSE ))  
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
  
  
  # -- add hash
  job_info <- append( job_info, c( "", "", paste( rep_len("-", length.out = 60 ), collapse = ""), paste0( "(hash: ", .self$hash(), ")" ) ) )
  
  
  # -- display job
  cat( c( "", job_info, "" ), sep = "\n")
  
  
})






