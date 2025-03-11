#' A reference class representing a cxlib batch job
#' 
#' @field .attr Internal storage
#' 
#' @method initialize initialize
#' @method submit submit
#' @method status status
#' @method actions actions
#' @method show show
#' 
#' 
#' @description
#' A utility class representing a cxlib batch job. A job can be identified by 
#' an existing Job ID or a vector of program paths or a job definition to execute.
#' 
#' A batch job consists of a set of actions, which in the current implementation
#' is limited to an ordered sequence of one or more programs. The programs can 
#' be specified as a character vector or as the named list entry `programs` as
#' part of the job definition. 
#' 
#' \emph{Note: All paths, including program annotations, are relative to the
#'  current working directory.}
#' 
#' If the `cxlib_batchjob`is initialized with a single character string in the 
#' format of a valid Job ID, it is assumed the string represents an existing Job.
#'
#' A vector of program paths can be specified as input. The program paths in the 
#' specified order becomes a new Job definition.
#'
#' A job definition provided as input is a list of named entries, with only `programs` 
#' required . 
#' 
#' \itemize{
#'   \item `programs` a vector of program paths in the order of execution 
#'   \item `id` a predefined Job ID in UUID format (see \link[uuid]{UUIDgenerate})
#'   \item `options` sets one or more options (see below) for the submitted Job
#' 
#' }
#'  
#' The following options are supported as part of a Job definition or as a cxlib
#' property. If the option is defined as both part of the Job and a property, the 
#' job definition takes precedence.
#' 
#' \itemize{
#'   \item `LOGS` defines the standard directory path for logs
#'   \item `LOG.FILEEXT` defines the standard log file extension
#' } 
#' 
#' If a Job ID is omitted, a new Job ID will be assigned.
#' 
#' If a job with the specified ID exists. the `cxlib_batchjob()` class 
#' represents the current job. If a job with a specified ID does not exist, 
#' a job with the specified ID is created. 
#' 
#' 
#' The `cxlib_batchjob` methods and underlying functions uses the file system for
#' 
#' 
#' \emph{Options}
#' 
#' Options can be defined as part of a Job definition or through cxlib properties
#' (see \link{cxlib_config}).
#'  
#' The following options sets the locations of for processing. 
#' \itemize{
#'   \item `PATH`option specifies the root directory for batch job processes. If
#'   not defined, the sub-directory `.cxlib` in the R session temporary directory 
#'   (\link[base]{tempdir}) is used. 
#'   \item `WORKPATH` option specifies the work area directory for batch jobs. 
#'   If not defined, the directory `PATH` is used. The `WORKPATH` is the 
#'   parent directory to job working directories. To use the R session temporary
#'   directory as the working directory for Jobs different to the `PATH` directory,
#'   set `WORKPATH` equal to `$R_SESSION_TEMP` or `[env] R_SESSION_TEMP`.  
#' }
#' 
#'
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
                       "options" = list( "logs" = cfg$option( "LOGS", unset = NULL), 
                                         "log.fileext" = cfg$option( "LOG.FILEEXT", unset = "Rout" ) ),
                       "paths" = c( "paths" = c( "root" =  cxlib_root ) ) )

  

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
  # .self$.attr[["paths"]][".job"] <- file.path( .self$.attr[["paths"]]["job.area"], ".cx", fsep = "/" )
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
      
      jobdef_opts <- names(job_json)[ ! names(job_json) %in% c( "id", "paths", "inputs" ) ]
      
      for ( xopt in jobdef_opts )
        .self$.attr[[ xopt ]] <- job_json[[xopt]]
    }
    

    # - exit point if not a new job  
    return()
  }
  
  
  
  # -- processing options
  
  if ( "options" %in% base::tolower(names(x)) ) 
    for ( xopt in base::tolower(names(x[["options"]])) )
      .self$.attr[["options"]][[ xopt ]] <- x[["options"]][[ utils::head( base::match( xopt, base::tolower(names(x[["options"]]))), n = 1 ) ]]
  
  
  
  # -- actions
  
  # - initialize list of actions
  action_lst <- list()
  

  # - actions where specified
  if ( "actions" %in% base::tolower(names(x)) )
    stop( "Actions specified as part of a job definition currently not supported" )

  
  
  # - default is current working directory  
  src_path <- base::getwd()


  
  # - using job archive as source location for files
  
  if ( "archive" %in% base::tolower(names(x)) ) {

    if ( ! "programs" %in% base::tolower(names(x)) )
      stop( "Named vector of programs required with job archive")
    
    
    # - unpack archive
    

    # - amend action list    

    
    # - use work area as source path for later processes
    src_path <- .self$.attr[["paths"]]["work.area"]
    
  }  # end of if-statement for job archive as source location
  
  
  
  # - local file system
  
  if ( ! "archive" %in% base::tolower(names(x)) ) {

    pgm_lst <- character(0)
    
    if ( "programs" %in% base::tolower(names(x)) )
      pgm_lst <- unname( x[["programs"]] )
    else 
      pgm_lst <- x
    

    if ( ! all( file.exists( file.path( cxlib::cxlib_standardpath(base::getwd()), pgm_lst, fsep = "/" ) ) ) )
      stop( "One or more specified programs do not exist" )
    
    # - stage programs in work area
    
    for ( xpgm in base::unique(pgm_lst) ) {
      
      pgm_src  <- file.path( cxlib::cxlib_standardpath(base::getwd()), xpgm, fsep = "/" )
      pgm_trgt <- file.path( .self$.attr[["paths"]]["work.area"], xpgm, fsep = "/" )
      
      pgm_src_sha1 <- digest::digest( pgm_src, algo = "sha1", file = TRUE )
      
      if ( ! dir.exists( base::dirname(pgm_trgt) ) && ! dir.create( base::dirname(pgm_trgt), recursive = TRUE ) )
        stop( "Could not stage program parent directory in job work area" )

      
      if ( ! file.copy( pgm_src, base::dirname(pgm_trgt), copy.mode = FALSE, copy.date = FALSE) || 
           ( digest::digest( pgm_trgt, algo = "sha1", file = TRUE ) != pgm_src_sha1 ) )
        stop( "Could not stage program ", xpgm )
      
      
      
      action_lst[[ length(action_lst) + 1 ]] <- list( "id" = cxlib:::.cxlib_referenceid( type = "action"),
                                                      "type" = "program",
                                                      "path" = xpgm,
                                                      "sha1" = pgm_src_sha1 )
      
      
      # reset critical objects
      rm ( list = c( "pgm_src", "pgm_trgt", "pgm_src_sha1" ) )

    } # end of for-statement across list of programs

  }  # end of if-statement for local file system as source location 


  
  # - futility on no actions
  if ( length(action_lst) == 0 )
    stop( "No actions defined" )
  
  
  
  
  # - update actions of type program
  
  for ( xidx in 1:length(action_lst) ) {
    
    if ( action_lst[[ xidx ]][["type"]] != "program" ) 
      next()
    
    
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
    
    
    if ( ! "archive" %in% names(x) ) {
      
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

  }
  
  
  
  # - process annotations
  
  for ( xidx in 1:length(action_lst) ) {
    
    
    # add program annotations
    if ( ! "annotations" %in% names(action_lst[[xidx]]) && action_lst[[ xidx ]][["type"]] == "program" ) 
      action_lst[[xidx]][["annotations"]] <- cxlib:::.cxlib_programannotations( file.path( .self$.attr[["paths"]]["work.area"], 
                                                                                           action_lst[[xidx]][["path"]], 
                                                                                           fsep = "/" ) )

    # check for nothing to do
    if ( ! "annotations" %in% names(action_lst[[xidx]]) )
      next()
    
    
    # stage annotated input
    if ( "input" %in% names(action_lst[[xidx]][["annotations"]]) )
      if ( length( action_lst[[xidx]][["annotations"]][["input"]] ) > 0 )
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
  
  
  
  # -- generate work area inventory
  
  .self$.attr[[ "inputs" ]] <- lapply( sort(cxlib::cxlib_standardpath(list.files( .self$.attr[["paths"]]["work.area"],
                                                                                  recursive = TRUE, 
                                                                                  all.files = TRUE, 
                                                                                  include.dirs = FALSE ))),
                                       function(x) {
                                         list( "path" = x, 
                                               "sha1" = digest::digest( file.path( .self$.attr[["paths"]]["work.area"], x, fsep = "/" ), algo = "sha1", file = TRUE ) )
                                       })
  
  
  
  
  # -- save job actions to job area
  
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
  
  
  # -- save job definition
  
  jobdef <- .self$.attr[ c( "id", "options", "inputs" ) ]
  
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
    
    xaction <- jsonlite::fromJSON( xfile, flatten = TRUE )
    
    xaction[["status"]] <- "planned"
    
    if ( tools::file_ext( xfile) == "lck" )
      xaction[["status"]] <- "executing"
    
    if ( grepl( "-completed.json$", xfile, ignore.case = TRUE, perl = TRUE) )
      xaction[["status"]] <- "completed"
    
    actions[[ length(actions) + 1 ]] <- xaction
    
  }
  

  return(invisible(actions))
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
                   

  # - add execution routine
  job_r <- append( job_r, 
                   " ",
                   "# execute job actions", 
                   paste0( "cxlib:::.cxlib_batchjob_execd( \"", .self$.attr[["paths"]][[".job"]], "\", work = \"", .self$.attr[["paths"]]["work.area"], "\" )" ) )
  
  
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







cxlib_batchjob$methods( "publish" = function() {
  "Save job results"
})



cxlib_batchjob$methods( "archive" = function() {
  "Create an archive of job results"
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
  