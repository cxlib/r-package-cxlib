#' (Experimental) Utility function to store R Compute Service personal access token 
#' 
#' @return Invisible NULL
#' 
#' @description
#' The R Compute Service (rcx) utilities access tokens to identify a permitted 
#' requester. The access token is stored in the users home directory.
#' 
#' 
#' @export


cxlib_remote_addtoken <- function() {
  
  # -- token store
  xpath <- cxlib::cxlib_standardpath( file.path( "~", ".local", "rcx", "token", fsep = "/" ) )
  
  
  if ( ! dir.exists(base::dirname(xpath)) && ! dir.create(base::dirname(xpath), recursive = TRUE) )
    stop( "Could not initiate the token store" )
  
  
  if ( inherits( base::writeLines( askpass::askpass( prompt = "R Compute Service (rcx) token" ), con = xpath), "try-error") )
    stop( "Could not store token" )
  
  return(invisible(NULL))
}