#' Simple utility to execute an R program script
#' 
#' @param x A job
#' @param options List of options for executing R programs
#' @param silent Disable verbose messaging
#' 
#' @return Invisible list of actions
#' 
#' @description
#' 
#' A job is a list of tasks to be executed. In its simplest form, it one or more 
#' programs that executes in batch mode. 
#' 
#' A job is executed in an isolated, temporary and transient working area.
#' 
#' Note: All paths are relative to the current R session working directory, i.e.
#' `getwd()`.
#'  
#' Program required inputs and output locations are annotated in the program file
#' using standard annotation syntax anywhere in the program file. Each annotation 
#' is defined on a separate line starting with the comment character `#`. One or 
#' more spaces separate the annotation keyword and its value.
#' 
#' Program input is specified using `@cx.input` followed by the input directory or
#' file path. If the input is a directory, all files in that directory is 
#' included. 
#' 
#' Program output locations is specified using the `@cx.output` annotation. It is 
#' assumed that the output location is a directory.
#' 
#' Only files created, updated or deleted in an annotated output directory are
#' returned in addition to the program log.
#' 
#' The function supports different execution options.
#' 
#' \itemize{
#'   \item `logs` option is the directory where logs are stored. If the option is 
#'         `NULL`, the program directory is used. 
#'   \item `log.fileext` specifies te log file extension or suffix. Default is the
#'         standard `Rout`. 
#' }
#' 
#' The options can also be defined in the `cxlib.properties` file 
#' (see \link[cxlib]{cxlib_config}. 
#' 
#' Options specified in the call to `cxlib_batch` takes precedence over options
#' defined in cxlib properties.
#' 
#' 
#' @examples
#' 
#' \dontrun{
#' # note: all paths are relative to the working directory
#' 
#'  
#' # -- run an R program
#' # not run
#' 
#' cxlib_batch( "path/to/my/program.R" )
#' 
#' 
#' # -- run a list of R programs
#' # not run
#' 
#' program_list <- c( "path/to/programs/program_1.R", "path/to/programs/program_2.R" )
#' cxlib_batch( program_list )
#'
#'
#' # -- alternate folder for logs
#' # not run
#' 
#' cxlib_batch( "path/to/my/program.R", options = list( "logs" = "path/to/logs" ) )
#' 
#'
#' # -- alternate file extension for logs
#' # not run
#' 
#' cxlib_batch( "path/to/my/program.R", options = list( "log.fileext" = "log" ) )
# 
#' }
#' 
#' @export


cxlib_batch <- function( x, options = list( "logs" = NULL, "log.fileext" = "Rout" ), silent = FALSE ) {


  # -- initiate job object
  job <- cxlib_batchjob( list( "programs" = x, "options" = options ) )
  
  
  # -- submit job
  job$submit( wait = TRUE )
  
  
  # -- job details
  job_details <- job$details()
  

  
  # -- save results
  job$save()
  

  # -- delete job
  job$delete()

  
  if ( silent )
    return(invisible(job_details))
  
  
  # -- show action result status
  
  msgs <- paste( "Job ID ", job_details[["id"]] )
  
  
  # - inputs
  
  if ( ! "inputs" %in% names(job_details) )
    msgs <- append( msgs, c( " ", "No input files identified" ) )
  
  if ( "inputs" %in% names(job_details) ) {
   
    msgs <- append( msgs, c( " ", 
                             "Inputs", 
                             paste( base::rep_len("-", 60), collapse = "") ) )
    
     
    for ( xinput in job_details[["inputs"]] )
      msgs <- append( msgs, 
                      c( xinput[["path"]],
                         paste0( "(SHA-1: ", xinput[["sha1"]], ")"), 
                         " ") )

    msgs <- append( msgs, c( "-- end of inputs -------", " " ) )
                    
  }

  
  
  # - actions  
  if ( ! "actions" %in% names(job_details) )
    msgs <- append( msgs, c( " ", "No actions registered" ) )
    
  if ( "actions" %in% names(job_details) )
    for ( xidx in 1:length(job_details[["actions"]]) ) {
      
      xact <- job_details[["actions"]][[ xidx ]]
      
      msgs <- append( msgs, 
                      c( " ",
                         paste0( "#", xidx, " ", xact[["type"]] ), 
                         paste( base::rep_len("-", 60), collapse = ""),
                         xact[["path"]],
                         paste0( "(SHA-1: ", xact[["sha1"]], ")") ) )
      
      if ( xact[["type"]] == "program" )
        msgs <- append( msgs, 
                        c( " ", 
                           paste0( "Log  ", xact[["log"]][["path"]]), 
                           paste0( "     (SHA-1: ", xact[["log"]][["sha1"]], ")" ), 
                           " " ) )

  }
    
  
  # - display messaging
  cat( msgs, sep = "\n" )
    

  return(invisible(job_details))
}
  
