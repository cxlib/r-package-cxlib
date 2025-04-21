#' Utility function to run an R program 
#' 
#' @param x Program path
#' @param log Alternative log path
#' @param wd Working directory
#' @param libpaths Vector of library paths
#' @param environ Custom environmental file
#' @param wait Wait for program to finish executing
#' 
#' 
#' 
#' @export 


cxlib_rcmd <- function( x, log = NULL, wd = base::getwd(), libpaths = .libPaths(), environ = NULL, wait = FALSE ) {
  
  
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

  # - change to working directory
  os_cmd <- paste( "cd", wd )
  
  # - add R call
  os_cmd <- append( os_cmd, paste( r_cmd, collapse = " ") )
    
  
  # - generate command string
  os_cmd_str <- paste( os_cmd, collapse = ";")
  
  
  rc <- system( os_cmd_str, wait = FALSE, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE )
  
    
  # if ( ! wait )
  #   os_cmd_str <- paste( "nohup", os_cmd_str, "&", sep = " " )
  # rc <- system( paste( "(", os_cmd_str, " &)"), intern = TRUE )
  

  print(rc)
    
}
