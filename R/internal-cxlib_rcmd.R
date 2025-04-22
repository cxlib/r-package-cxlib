#' Internal utility function to run an R program as a background process
#' 
#' @param x Program path
#' @param log Alternative log path
#' @param wd Working directory
#' 
#' @return Invisible NULL
#' 
#' @description
#' A crude function to execute an R program as a standalone process 
#' 
#' The .Renviron configuration can be coerced using the cxlib property
#' `RENVIRON.DEFAULT` to define standard R compute environment configuration.
#' 
#' 
#' @keywords internal


.cxlib_rcmd <- function( x, log = NULL, wd = base::getwd() ) {
  
  
  if ( missing(x) || is.null(x) || any(is.na(x)) || ! inherits( x, "character" ) || (length(x) != 1) )
    stop( "Program missing or invalid" )
  
  
  if ( is.null(wd) || any(is.na(wd)) || ! inherits( wd, "character" ) || (length(wd) != 1) || ! dir.exists(wd) )
    stop( "Working directory missing or invalid" )
  
  
  
  # -- program
  pgm <- cxlib::cxlib_standardpath(x)

  if ( ! file.exists(pgm) )
    stop("Program does not exist")
  
  
  # -- log path
  log_path <- paste0( tools::file_path_sans_ext(pgm), ".Rout" )
  
  if ( ! is.null(log) )
    log_path <- cxlib::cxlib_standardpath(log) 
  
  
  
  
  

  # -- R command
  
  # - initialize with BATCH commands
  r_cmd <- file.path( base::R.home( component = "bin" ), "BATCH", fsep = "/" )  
  
  # - add environment controls
  r_cmd <- append( r_cmd, c( "--no-save", "--no-restore" ) )
  
  # - add program
  r_cmd <- append( r_cmd, pgm )

  # - add log
  r_cmd <- append( r_cmd, log_path )
  
  
  
  # -- OS command 

  os_cmd <- character(0)
  
  
  # - environ
  cfg <- cxlib::cxlib_config()
  
  if ( ! is.na(cfg$option( "ENVIRON.DEFAULT", unset = NA ) ) )
    os_cmd <- append( os_cmd, 
                      paste0("export R_ENVIRON_USER=",cfg$option( "ENVIRON.DEFAULT", unset = "" ) ) )

  
  # - change to working directory
  os_cmd <- append( os_cmd, paste( "cd", wd ) )

  
  # - add R call
  os_cmd <- append( os_cmd, paste( r_cmd, collapse = " ") )
    
  
  # - generate command string
  os_cmd_str <- paste( os_cmd, collapse = ";")
  
  
  system( os_cmd_str, wait = FALSE, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE )
  
  
  return(invisible(NULL))
}