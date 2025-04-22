#' (Experimental) Internal function to read R Compute Service token
#' 
#' @return Invisible character value 
#' 
#' @keywords internal

.cxlib_remote_accesstoken <- function() {

  # -- token store
  xpath <- cxlib::cxlib_standardpath( file.path( "~", ".local", "rcx", "token", fsep = "/" ) )
  
  if ( ! file.exists(xpath) )
    stop( "No token saved for R Compute Service" )
  
  
  return(invisible( base::suppressWarnings(base::readLines(xpath, warn = FALSE )) ))
}