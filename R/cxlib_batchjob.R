#' A reference class representing a cxlib batch job
#' 
#' @field .attr Internal storage
#' 
#' @method initialize initialize
#' @method add add
#' @method submit submit
#' @method status status
#' @method details details
#' @method actions actions
#' @method show show
#' 
#' 
#' @description
#' A utility class representing a cxlib batch job. This utility is not accessed 
#' directly but the mechanisms and methods for \link[cxlib]{cxlib_batch} and services
#' implementing R jobs and associated actions. 
#' 
#' A job can be identified by an existing Job ID or a vector of program paths or
#' a job definition to execute.
#' 
#' A batch job consists of a set of actions, which in the current implementation
#' is limited to an ordered sequence of one or more programs. The programs can 
#' be specified as a character vector or as the named list entry `programs` as
#' part of the job definition. 
#' 
#' \emph{Note: All paths, including program annotations, are relative to the
#'  current working directory.}
#' 
#' The job actions can be specified as part of object initialization, using the 
#' `add(x)` method or both. Note that an added action cannot be removed and 
#' actions added to a job with existing actions will append the actions to the 
#' existing list.
#' 
#' If `cxlib_batchjob` is initialized with a single character string in the 
#' format of a valid Job ID, it is assumed the string represents an existing Job.
#'
#' A vector of program paths can be specified as input during initialization or 
#' to the `add()`method. The program paths in the specified order becomes the
#' new Job definition.
#'
#' A job definition can be provided as input in the form of a list of named entries, 
#' with only `programs` required. 
#' 
#' \itemize{
#'   \item `programs` a vector of program paths in the order of execution 
#'   \item `id` a predefined Job ID in UUID format (see \link[uuid]{UUIDgenerate})
#'   \item `archive` with inputs and programs provided as the contents of a job archive
#'   \item `options` sets one or more options (see below) for the submitted Job
#' }
#'
#' A job `archive` is a Zip file (see \link[zip]{zip}) that contains all programs 
#' and required input files. The root of the Zip-file internal directory structure
#' represents the working directory for the executing actions. 
#'  
#' The following options are supported as part of a Job definition or as a cxlib
#' property (see process options below). Options can only be defined during
#' `cxlib_batchjob()` initialization.
#' 
#' \itemize{
#'   \item `LOGS` defines the standard directory path for logs
#'   \item `LOG.FILEEXT` defines the standard log file extension
#' } 
#' 
#' If the option is defined as both part of the Job and a property, the job
#' definition takes precedence.
#' 
#' Processing options can be defined as part of a Job definition or through cxlib
#' properties (see \link[cxlib]{cxlib_config}).
#'  
#' The following options sets the locations of for processing. 
#' \itemize{
#'   \item `PATH`option specifies the root directory for batch job processes. If
#'   not defined, the sub-directory `.cxlib` in the R session temporary directory 
#'   (\link[base]{tempdir}) is used. 
#'   \item `WORKPATH` option specifies the work area directory for batch jobs. 
#'   If not defined, the directory `PATH` is used. The `WORKPATH` is the 
#'   parent directory to job working directories.  
#' }
#' 
#' The method `submit( wait = TRUE )` submits the registered actions for processing.
#' Actions are executed in a standalone R session. If `WAIT` is equal to `FALSE`,
#' the R session is executed in the background.
#'
#' The `save()` methods saves the action results to the current working directory
#' on the R session local file system.
#' 
#' The `archive()` method creates a standard Zip-file archive `job-<id>-results.zip` 
#' containing the results of the job actions, with root of the Zip-file 
#' representing the job working directory. The method invisibly returns the
#' path to the Zip-file archive.
#' 
#' The `delete()` method deletes the temporary directories associated with the job.
#'
#' The experimental method `actions()` returns a list of actions and their status.
#' 
#' The experimental method `status()` returns a list of actions with abbreviated
#' information including their status.
#' 
#' The experimental method `details()` returns a list of job details including
#' job actions and status.
#' 
#' 
#' 
#' @exportClass cxlib_batchjob
#' @export cxlib_batchjob


cxlib_batchjob <- methods::setRefClass( "cxlib_batchjob", 
                                        fields = list( ".attr" = "list") )


cxlib_batchjob$methods( "initialize" = function( x ) {
  "Create new batch job"
  
  
  if ( missing(x) || is.null(x) || all(is.na(x)) || (length(x) == 0) || ! inherits( x, c("character", "list") ) )
    stop( "Input is invalid or missing" )
  
  

  # -- configuration options
  
  cfg <- cxlib::cxlib_config()

  # - check PATH if set
  if ( ! is.na(cfg$option( "PATH", unset = NA )) && ! dir.exists(cfg$option( "PATH", unset = NA )) )
    stop( "The PATH configuration option directory ", cfg$option( "PATH", unset = NA ), " does not exist" )
  
  # - check WORKPATH if set
  if ( ! is.na(cfg$option( "WORKPATH", unset = NA )) && ! dir.exists(cfg$option( "WORKPATH", unset = NA )) )
    stop( "The WORKPATH configuration option directory ", cfg$option( "WORKPATH", unset = NA ), " does not exist" )
  
  

  
  # -- define the internal root directory path
  
  cxlib_root <- cxlib::cxlib_standardpath( cfg$option( "PATH", unset = file.path( base::tempdir(), ".cxlib" ) ) )

  # - note: using unset = NA to indicate if it is set even though we already have derived cxlib_root  
  if ( is.na(cfg$option( "PATH", unset = NA )) && ! dir.exists( cxlib_root ) && ! dir.create( cxlib_root, recursive = TRUE ) )
      stop( "Could not initiate defualt path" )

  

  # -- initialize internals 
  
  .self$.attr <- list( "id" = cxlib:::.cxlib_referenceid( type = "job" ), 
                       "mode" = "source",
                       "options" = list( "logs" = cfg$option( "LOGS", unset = NULL), 
                                         "log.fileext" = cfg$option( "LOG.FILEEXT", unset = "Rout" ) ),
                       "paths" = c( "root" =  cxlib_root ) )

  

  # -- default assumption of job state 
  as_new <- TRUE
  
  
  
  # -- job ID
  
  # - as an existing job id
  if ( ! missing(x) && ! is.null(x) && inherits(x, "character") && (length(x) == 1) && all(uuid::UUIDvalidate(x)) ) {
    # not a new job
    
    as_new <- FALSE

    .self$.attr[["id"]] <- utils::head( x, n = 1)
  }
  
  
  # - as part of a job definition
  if ( ! missing(x) && ! is.null(x) && 
       inherits(x, c( "character", "list") ) && (length(x) >= 1) && "id" %in% base::tolower(names(x)) ) {
    # as a new job
    
    as_new <- TRUE

    .self$.attr[["id"]] <- x[[ utils::head( base::match( "id", base::tolower(names(x))), n = 1 ) ]]
  }

        
  # - verify ID is valid
  #   note: if it is the default value this is obviously TRUE
  #   note: only case it can be FALSE is if ID is specified
  if ( ! uuid::UUIDvalidate( .self$.attr[["id"]] ) ) 
    stop( "Specified Job ID is in an invalid format" )


  # - check for conflicts
  
  if ( ! as_new && ! dir.exists( file.path( cxlib_root, .self$.attr[["id"]], fsep = "/" ) ) )
    stop( "A job with the specified ID is not defined" )
  
  if ( as_new && dir.exists( file.path( cxlib_root, .self$.attr[["id"]], fsep = "/" ) ) )
    stop( "A job with the specified ID already exists" )
  
  
  
  
  # -- define job directory structure

  # - job internals
  .self$.attr[["paths"]][".job"] <- file.path( cxlib_root, .self$.attr[["id"]], ".job", fsep = "/" )
  
  
  # - job working directory
  #   note: the job directory work area will be <WORKPATH | cxlib_root >/<job id>/.work

  .self$.attr[["paths"]]["work.area"] <- file.path( cxlib::cxlib_standardpath( cfg$option( "WORKPATH", unset = cxlib_root ) ),
                                                    .self$.attr[["id"]],
                                                    ".work",
                                                    fsep = "/" )
  
  # - set up/heal directory structure
  for ( xarea in c( ".job", "work.area" ) )
    if ( ! dir.exists( .self$.attr[["paths"]][xarea] ) && ! dir.create( .self$.attr[["paths"]][xarea], recursive = TRUE) )
      stop( "Could not initiate/re-build job areas" )
  

  
  # -- not a new job
  
  if ( ! as_new ) {
  
    # - restore job definition
    if ( file.exists( file.path( .self$.attr[["paths"]][".job"], "job.json", fsep = "/") ) ) {
      
      job_json <- jsonlite::fromJSON( file.path( .self$.attr[["paths"]][".job"], "job.json", fsep = "/") )
      
      jobdef_opts <- names(job_json)[ ! names(job_json) %in% c( "id", "paths" ) ]
      
      for ( xopt in jobdef_opts )
        .self$.attr[[ xopt ]] <- job_json[[xopt]]
    }
    

    # - exit point if not a new job  
    return()
  }
  
  
  # -- input mode
  
  if ( "mode" %in% names(x) )
    .self$.attr[["mode"]] <- unname(x[["mode"]])
  
  if ( "archive" %in% names(x) )
    .self$.attr[["mode"]] <- "archive"
  
  

  # -- processing options
  
  if ( "options" %in% base::tolower(names(x)) ) 
    for ( xopt in base::tolower(names(x[["options"]])) )
      .self$.attr[["options"]][[ xopt ]] <- x[["options"]][[ utils::head( base::match( xopt, base::tolower(names(x[["options"]]))), n = 1 ) ]]
  

  
  # -- save initial job definition

  jobdef <- .self$.attr[ c( "id", "mode", "options" ) ]
  
  base::writeLines( jsonlite::toJSON( jobdef, pretty = TRUE, null = "null" ), 
                    con = file.path( .self$.attr[["paths"]][".job"], "job.json", fsep = "/" ) )
  
  
  
  
  # -- add actions to job 
  #    note: exclude options
  
  if ( is.null(names(x)) )
    .self$add( x )
  else
    .self$add( x[ ! names(x) %in% "options" ] )

})




cxlib_batchjob$methods( "add" = function(x) {
  "Add job actions"


  # -- initialize list of actions
  action_lst <- .self$actions()
  

  # note: for now, amending executing jobs is not permitted  
  for ( xact in action_lst ) 
    if ( xact[["status"]] != "planned" )
      stop( "Cannot add to a job that is currently being processed" )

  
    
  # -- actions
  
  # - actions where specified
  if ( "actions" %in% base::tolower(names(x)) )
    stop( "Actions specified as part of a job definition currently not supported" )
  
  
  
    
  # -- archive
  
  if ( "archive" %in% base::tolower(names(x)) ) {
    
    if ( ! file.exists( x[["archive"]] ) )
      stop( "The archive file does not exist" )

        
    # - use work area as source path for later processes
    src_path <- .self$.attr[["paths"]]["work.area"]
    
    
    # - unpack archive
    zip::unzip( x[["archive"]], overwrite = TRUE, exdir = src_path )
    
  } # end if-statement on archive
  

  
  # -- programs
  
  if ( is.null(names(x)) || "programs" %in% names(x) || any( base::trimws(names(x)) == "" ) ) {
    
    add_programs <- character(0)
    
    if ( is.null(names(x)) )
      add_programs <- unlist( x, use.names = FALSE )
    
    if ( "programs" %in% names(x) )
      add_programs <- x[["programs"]]
    
    if ( ! "programs" %in% names(x) && any( base::trimws(names(x)) == "" ) )
      add_programs <- x[[ base::trimws(names(x)) == "" ]]
    
    
    
    # - add programs
    
    for ( xpgm in add_programs ) {
  
      program_action <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"),
                              "type" = "program", 
                              "path" = xpgm )
      
      
      if ( ( .self$.attr[["mode"]] == "source" ) ) {
        
        pgm_xpath <- file.path( cxlib::cxlib_standardpath(base::getwd()), xpgm, fsep = "/" )
        
        if ( ! file.exists( pgm_xpath ) )
          stop( "Program ", xpgm, " does not exist" )

        program_action[["sha1"]] <- digest::digest( pgm_xpath, alg = "sha1", file = TRUE )
        
        
        # - stage program
        
        pgm_trgt <- file.path(  .self$.attr[["paths"]]["work.area"], xpgm, fsep = "/" )
        
        if ( ! dir.exists( base::dirname(pgm_trgt)) && ! dir.create( base::dirname(pgm_trgt), recursive = TRUE) )
          stop( "Could not stage program parent directory in the job work area" )

        
        if ( file.exists(pgm_trgt) )
          stop( "Program ", xpgm, " already exists in work area" )

        
        if ( ! file.copy( pgm_xpath, base::dirname(pgm_trgt), copy.mode = FALSE, copy.date = FALSE, overwrite = TRUE ) ||
             ! file.exists( pgm_trgt ) ||
             (digest::digest( pgm_trgt, algo = "sha1", file = TRUE ) != program_action[["sha1"]] ) )
          stop( "Failed to stage program source" )
        
                
      } # end of if-statement for program under mode source
      
        
      
      # - add program action to the action list
      action_lst[[ length(action_lst) + 1 ]] <- program_action

    }  # end of for-statement for adding program actions

  } # end of if-statement for programs
  

  
  # -- update SHA-1 and log for program actions

  if ( length(action_lst) > 0 )
    for ( xidx in 1:length(action_lst) ) {
    
      if ( action_lst[[ xidx ]][["type"]] != "program" ) 
        next()
      
      
      # - amend SHA-1 if not existing
      
      if ( ! "sha1" %in% names(action_lst[[xidx]]) &&
           file.exists( file.path( .self$.attr[["paths"]]["work.area"], xpgm, fsep = "/" ) ) ) 
        action_lst[[xidx]][["sha1"]] <- digest::digest( file.path( .self$.attr[["paths"]]["work.area"], action_lst[[ xidx ]][["path"]], fsep = "/" ), alg = "sha1", file = TRUE )
  
  
      # . amend log details
      
      if ( ! "log" %in% names(action_lst[[xidx]]) )
        action_lst[[xidx]][["log"]] <- list()
      
      
      if ( ! "path" %in% names(action_lst[[xidx]][["log"]]) ) {
        
        log_parent <- ifelse( is.null(.self$.attr[["options"]][["logs"]]), 
                              base::dirname( action_lst[[xidx]][["path"]] ), 
                              .self$.attr[["options"]][["logs"]] )
        
        log_file <- character(0)
        
        if ( is.null( .self$.attr[["options"]][["logs"]] ) )
          log_file <- paste0( tools::file_path_sans_ext(action_lst[[xidx]][["path"]]), ".", .self$.attr[["options"]][["log.fileext"]] )
        else
          log_file <- file.path( .self$.attr[["options"]][["logs"]], 
                                 paste0( tools::file_path_sans_ext(base::basename(action_lst[[xidx]][["path"]])), 
                                         ".", 
                                         .self$.attr[["options"]][["log.fileext"]] ) )
        
        action_lst[[xidx]][["log"]][["path"]] <- log_file
        action_lst[[xidx]][["log"]][["sha1"]] <- NA
      }
      
      
      if ( .self$.attr[["mode"]] == "source" ) {
  
        log_src <- file.path( cxlib::cxlib_standardpath(base::getwd()), action_lst[[xidx]][["log"]][["path"]], fsep = "/" )
        
        # - make sure log directory exists in source location (write back at some point)
        if ( ! dir.exists( base::dirname( log_src ) ) )
          stop( "The log directory does not exist in the current working directory" )
        
        
        # - reference sha1 if not an archive
        if ( file.exists( log_src ) )
          action_lst[[xidx]][["log"]][["reference.sha1"]] <- digest::digest( log_src, algo = "sha1", file = TRUE )
        
      }
      
      
      # - set up log directory in work area
      
      log_workarea_dir <- base::dirname( file.path( .self$.attr[["paths"]]["work.area"], action_lst[[xidx]][["log"]][["path"]], fsep = "/" ) )
      
      if ( ! dir.exists( log_workarea_dir ) && ! dir.create( log_workarea_dir, recursive = TRUE ) )
        stop( "Could not stage log directory in job work area" )
      
    }  # end of for-statement to update log for action entry of type program
    
  
  
  
  # - process annotations
  
  if ( length(action_lst) > 0 )
    for ( xidx in 1:length(action_lst) ) {
    
      
      # add program annotations
      if ( ! "annotations" %in% names(action_lst[[xidx]]) && ( action_lst[[ xidx ]][["type"]] == "program" ) ) {
        
        xpgm_path <- file.path( .self$.attr[["paths"]]["work.area"], action_lst[[xidx]][["path"]], fsep = "/" )
        
        if ( file.exists( xpgm_path ) )
          action_lst[[xidx]][["annotations"]] <- cxlib:::.cxlib_programannotations( xpgm_path )
        
      } # end of if-statement for misisng annotations for a program
  
        
      
      # check for nothing to do
      if ( ! "annotations" %in% names(action_lst[[xidx]]) || (length(action_lst[[xidx]][["annotations"]]) == 0 ) )
        next()
      
      
      
      # stage annotated input
      # note: only applicable if it is not from an archive
      
      if (  ( .self$.attr[["mode"]] == "source" ) && 
            "input" %in% names(action_lst[[xidx]][["annotations"]]) &&
            ( length( action_lst[[xidx]][["annotations"]][["input"]] ) > 0 ) )
        for ( xinput in action_lst[[xidx]][["annotations"]][["input"]] ) {
          
          input_files <- character(0)
          
          xinput_src <- file.path( cxlib::cxlib_standardpath(base::getwd()), xinput, fsep = "/" )
          
          # - input as a file          
          if ( file.exists( xinput_src ) && ! dir.exists( xinput_src ) )
            input_files <- base::unique( append(input_files, xinput ) )
          
          # - input as a directory
          if ( dir.exists( xinput_src ) ) 
            input_files <- base::unique( append( input_files, 
                                                 file.path( xinput, 
                                                            list.files( xinput_src, 
                                                                        full.names = FALSE, 
                                                                        recursive = FALSE, 
                                                                        all.files = FALSE, 
                                                                        include.dirs = FALSE ) ) ) )
          
          
          if ( length(input_files) == 0 ) {
            base::rm( list = "input_files" )
            next()
          }
          
          
          # - stage parent directories
          for ( xpath in base::unique(base::dirname( input_files )) )
            if ( ! dir.exists( file.path( .self$.attr[["paths"]]["work.area"], xpath, fsep = "/" ) ) &&
                 ! dir.create( file.path( .self$.attr[["paths"]]["work.area"], xpath, fsep = "/" ), recursive = TRUE ) )
              stop( "Could not stage parent directory to input" )
          
          
          # note: assumption file exists
          for ( xfile in input_files ) {
            
            xsrc  <- file.path( cxlib::cxlib_standardpath(base::getwd()), xfile, fsep = "/" )
            xtrgt <- file.path( .self$.attr[["paths"]]["work.area"], xfile, fsep = "/" )
            
            # - check for overwrite conflicts (very very very very defensive)
            if ( file.exists( xtrgt ) &&
                 ( digest::digest( xsrc, algo = "sha1", file = TRUE ) != digest::digest( xtrgt, algo = "sha1", file = TRUE ) ) )
              stop( "Attempting to overwrite ", xfile, " with a different version (SHA-1 missmatch)" )
            
            
            # - file exists and not overwriting anything
            if ( file.exists( xtrgt ) )
              next()
            
            
            # - stage file
            if ( ! file.copy( xsrc, base::dirname(xtrgt), copy.mode = FALSE, copy.date = FALSE ) ||
                 ( digest::digest( xsrc, algo = "sha1", file = TRUE ) != digest::digest( xtrgt, algo = "sha1", file = TRUE ) ) )
              stop( "Failed to properly stage input file ", xfile )
            
          }
          
        } # end of for-statement for input annotations
      
      
      # create annotated output directories
      if ( "output" %in% names(action_lst[[xidx]][["annotations"]]) )
        if ( length( action_lst[[xidx]][["annotations"]][["output"]] ) > 0 )
          for ( xout in action_lst[[xidx]][["annotations"]][["output"]] ) {
            
            xout_path <- cxlib::cxlib_standardpath( file.path( .self$.attr[["paths"]]["work.area"], xout, fsep = "/" ) )
            
            if ( ! dir.exists( xout_path ) && ! dir.create( xout_path, recursive = TRUE ) )
              stop( "Could not stage output directory" )
            
          }
      
      
    } # end of for-statement for action annotations  
  
  

  # -- update work area inventory
  
  .self$.attr[[ "inputs" ]] <- lapply( sort(cxlib::cxlib_standardpath(list.files( .self$.attr[["paths"]]["work.area"],
                                                                                  recursive = TRUE, 
                                                                                  all.files = TRUE, 
                                                                                  include.dirs = FALSE ))),
                                       function(x) {
                                         list( "path" = x, 
                                               "sha1" = digest::digest( file.path( .self$.attr[["paths"]]["work.area"], x, fsep = "/" ), algo = "sha1", file = TRUE ) )
                                       })
  
  
  
  
  # -- save job actions to job area
  
  if ( length(action_lst) > 0 ) {
    
    prefix_fmt <- paste0( "%0", as.character( floor(log10(length(action_lst))) + 2 ), "d" )
  
    for ( xi in 1:length(action_lst) ) {
      
      if ( ! "id" %in% names(action_lst[[xi]]) )
        action_lst[[xi]][["id"]] <- cxlib:::.cxlib_referenceid( type = "action" )
      
      
      action_file <- paste0( sprintf( prefix_fmt, xi ), 
                             "-action-", 
                             action_lst[[xi]][["type"]],
                             "-",
                             action_lst[[xi]][["id"]], 
                             ".json" )
      
      base::writeLines( jsonlite::toJSON( action_lst[[xi]], pretty = TRUE, null = "null" ), 
                        con = file.path( .self$.attr[["paths"]][".job"], action_file, fsep = "/" ) )
      
    }
    
  }  # end of if-statement for actions defined
  
  
  
  # -- save job definition
  
  jobdef <- .self$.attr[ c( "id", "mode", "options", "inputs" ) ]
  
  base::writeLines( jsonlite::toJSON( jobdef, pretty = TRUE, null = "null" ), 
                    con = file.path( .self$.attr[["paths"]][".job"], "job.json", fsep = "/" ) )
  
    
})




cxlib_batchjob$methods( "actions" = function() {
  "List actions"
  
  actions <- list()
  
  
  lst_files <- base::sort( list.files( .self$.attr[["paths"]][".job"], 
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
    
    actions[[ length(actions) + 1 ]] <- xaction
    
  }
  

  return(invisible(actions))
})



cxlib_batchjob$methods( "details" = function( wait = TRUE ) {
  "Job details" 
  
  # -- initialize with job details
  #    note: exclude paths 
  job_details <- .self$.attr[ ! names(.self$.attr) %in% "paths" ]
  
  
  # -- add job actions
  job_details[["actions"]] <- .self$actions()
  
  
  return(invisible(job_details))
})
  




cxlib_batchjob$methods( "submit" = function( wait = TRUE ) {
  "Submit job"

  
  # -- job environ
  # ... just a bit early on that one
  
    
  # -- job runner program 
  job_r_pgm <- file.path( .self$.attr[["paths"]][[".job"]], 
                          paste0( "job-", .self$.attr[["id"]], ".R"), 
                          fsep = "/")
  
  # - remove existing
  if ( file.exists( job_r_pgm ) )
    base::unlink( job_r_pgm, force = TRUE )
  
  if ( file.exists( job_r_pgm ) )
    stop( "Could not reset job runner" )
    

  # -- job runner log
  job_r_log <- paste0( tools::file_path_sans_ext( job_r_pgm), ".Rout" )

  
  
  # -- set up job runner
  job_r <- c( format( as.POSIXct( Sys.time(), tz = "UTC"), format = "# Job runner created %Y-%m-%d %H:%M:%S %Z" ), 
              paste( "# Job ID:  ", .self$.attr[["id"]] ) )
  

  # - transient working directory
  job_r <- append( job_r, 
                   c( " ",
                      "# transient working directory",
                      "base::setwd( base::tempdir() )" ) )

  
  # - results archive
  if ( wait ) 
    job_r <- append( job_r, 
                     c( " ",
                        "# do not archive results",
                        "archive_results <- FALSE" ) )
                        

  if ( ! wait ) 
    job_r <- append( job_r, 
                     c( " ",
                        "# archive results",
                        "archive_results <- TRUE" ) )
  

  
  # - add execution routine
  job_r <- append( job_r, 
                   c( " ",
                      "# execute job actions", 
                      paste0( "cxlib:::.cxlib_batchjob_execd( \"", .self$.attr[["paths"]][[".job"]], "\", work = \"", .self$.attr[["paths"]]["work.area"], "\", archive.results = archive_results )" ) ) )
  
  
  # - create job runner
  base::writeLines( job_r, con = job_r_pgm )
  
  
  if ( ! file.exists( job_r_pgm ) )
    stop( "Could not create job runner" )
  
  
  
  
  
  # -- batch execute program
  
  batch_args <- character(0)
  
  # - add R workspace flags
  batch_args <- append( batch_args, c( "--no-restore --no-save" ) )
  
  # - add program 
  batch_args <- append( batch_args, job_r_pgm )
  
  # - add log 
  batch_args <- append( batch_args, job_r_log )

  
  
  if ( wait ) {
  
    # - wait for the job runner to finish
    
    rc <- try( callr::rcmd( "BATCH",
                            cmdargs = batch_args,
                            wd = cxlib::cxlib_standardpath( base::tempdir() ),
                            echo = FALSE,
                            show = FALSE,
                            spinner = FALSE ),
               silent = TRUE )

    if ( inherits( rc, "try-error") )
      stop( "Failed to run job" )
        
    return()
  }
  
  
  
  # - submit the job runner to run in the background  
  
  rc <- try( callr::rcmd_bg( "BATCH",
                             cmdargs = batch_args,
                             wd = cxlib::cxlib_standardpath( base::tempdir() ) ),
             silent = TRUE )

  if ( inherits( rc, "try-error") )
    stop( "Failed to run job" )
  

})




cxlib_batchjob$methods( "status" = function() {
  "Job status"
  
  stat <- list()
  
  for ( xact in .self$actions() ) {
    
    # -- copy forward
    stat[[ length(stat) + 1 ]] <- xact[ c( "id", "type", "path", "sha1", "status") ]
    
    # -- log
    stat[[ length(stat) ]][["log"]] <- xact[["log"]][["path"]] 
    
  }
  
  
  return(invisible( stat ))
})







cxlib_batchjob$methods( "save" = function() {
  "Save job results"
  
  
  # -- get actions
  action_lst <- .self$actions()
  
  
  # -- futility ... integrity checks and actions completed 

  for ( xact in action_lst ) {
 
    # - action should be completed
    if ( ! "status" %in% names(xact) || ! xact[["status"]] %in% c( "complete", "completed" ) )
      stop( "Not all actions have completed")
  
    
    # - program action integrity
    
    if ( "type" %in% names(xact) && (base::toupper(xact[["type"]]) == "PROGRAM") ) {
      
      src_pgm <- file.path( cxlib::cxlib_standardpath(base::getwd()), xact[["path"]], fsep = "/" )
      
      if ( ! file.exists( src_pgm ) )
        stop( "The source for the program ", xact[["path"]], " no longer exists" )
      
      if ( ! "sha1" %in% names(xact) || ( digest::digest( src_pgm, algo = "sha1", file = TRUE ) != xact[["sha1"]] ) )
        stop( "The program ", xact[["path"]], " has changed since submitted or integrity cannot be verified" )

      if ( ! "log" %in% names(xact) || ! "path" %in% names(xact[["log"]]) )
        stop( "Expecting log details for a program action" )


      if ( ! file.exists( file.path( .self$.attr[["paths"]]["work.area"], xact[["log"]][["path"]], fsep = "/" ) ) )
        stop( "Expecting log file to exist as a result of a program action")
      
            
      src_log <- file.path( cxlib::cxlib_standardpath(base::getwd()), xact[["log"]][["path"]], fsep = "/" )

      if ( file.exists( src_log ) && 
           ( ! "reference.sha1" %in% names(xact[["log"]]) ||
             is.na(xact[["log"]][["reference.sha1"]]) ) )
        stop( "A new program log exists while it did not exist when the program action was submitted" )
      
      if ( "reference.sha1" %in% names(xact[["log"]]) &&
           ! is.na(xact[["log"]][["reference.sha1"]]) && 
           file.exists( src_log ) &&
           ( digest::digest( src_log, algo = "sha1", file = TRUE ) != xact[["log"]][["reference.sha1"]] ) )
        stop( "The program log in the output location has been updated and could indicate that the program was executed in parallel" )
      
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
  
  
  
  # -- delete files
  
  for ( xtodelete in files_to_delete ) {
    
    xtrgt <- file.path( cxlib::cxlib_standardpath(base::getwd()), xtodelete, fsep = "/" )
    
    if ( file.exists( xtrgt ) && ! file.remove( xtrgt ) )
      stop( "Failed to delete file ", xtodelete )

  }


    
  # -- write files

  for ( xtowrite in base::unique(append( logs_to_write, files_to_write  )) ) {
    
    xsrc  <- file.path( .self$.attr[["paths"]]["work.area"], xtowrite, fsep = "/" )
    xtrgt <- file.path( cxlib::cxlib_standardpath(base::getwd()), xtowrite, fsep = "/" )
    
    if ( ! dir.exists( base::dirname(xtrgt) ) )
      stop( "Saving file ", xtowrite, " failed as output directory does not exist" )
    
    if ( ! file.copy( xsrc, base::dirname(xtrgt), copy.mode = FALSE, copy.date = FALSE, overwrite = TRUE ) )
      stop( "Failed to save file ", xtodelete )
    
  }

  
})




cxlib_batchjob$methods( "archive" = function() {
  "Create an archive of job results"
  
  results_archive <- file.path( .self$.attr[["paths"]][".job"], paste0( "job-", .self$.attr[["id"]], "-results.zip" ), fsep = "/" )

  return(invisible( cxlib:::.cxlib_batchjob_resultsarchive( results_archive, job.path = .self$.attr[["paths"]][".job"], work.path = .self$.attr[["paths"]]["work.area"]) ))
})





cxlib_batchjob$methods( "delete" = function() {
  "Delete job areas"


  # recursively remove work areas
  
  for ( xarea in c( "work.area", ".job" ) ) {

    # important: parent directory contains the job ID
    base::unlink( base::dirname( .self$.attr[["paths"]][ xarea ] ), recursive = TRUE, force = TRUE )
  }

  return(invisible(NULL))
})




cxlib_batchjob$methods( "show" = function() {
  "Job information"
  
  # -- some labels
  type_to_action <- c( "program" = "Run program" )

  # -- default display information for job
  info <- c( " ", 
             paste( "cxlib job", .self$.attr[["id"]] ) )

  
  # -- add actions
  info <- append( info, 
                  c( " ",
                     "Actions",
                     paste( base::rep_len("-", 70), collapse = "" )) )
  
  for ( xstat in .self$status() ) { 
    
    if ( xstat[["type"]] == "program" ) 
      info <- append( info, 
                      c( " ", 
                         paste( type_to_action[ xstat[["type"]] ], xstat[["path"]], paste0( " [", xstat[["status"]]  ,"]") ),
                         paste0( "(SHA-1: ", xstat[["sha1"]], ")" )
                      ) )
    
    } # end of for-statement across status
    

  
  # -- display info in console
  cat( info, sep = "\n" )

})
  