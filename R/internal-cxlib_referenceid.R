#' Internal utility function to generate a unique reference iD of different types
#' 
#' @param type ID type
#' 
#' @return A character vector with the reference ID
#' 
#' @description
#' 
#' Type equal to `uuid` returns a secure random UUID
#' 
#' Type equal to `sha` or `raw` returns the SHA-1 of the UUID
#' 
#' Type equal to `NULL` returns formatted SHA-1 of the UUID
#' 
#' The `type` may also refer to the ID for a specific context. Valid contexts are
#' \itemize{
#'   \item `job` refers to the ID of a Job
#'   \item `action` refers to the ID of a Job action
#' }
#' 
#' 
#' 
#' 
#' @keywords internal

.cxlib_referenceid <- function( type = NULL ) {
  
  # -- some IDs
  
  # - generate ID
  refid <- uuid::UUIDgenerate()
  
  
  # - generate a SHA-1 of reference id
  sha <- digest::digest( refid, algo = "sha1", file = FALSE )

  
  # - formatted sha
  if ( base::nchar(sha) != 40 )
    stop( "SHA-1 has unexpected length" )
  
  str <- character(0)
  
  for ( i in 1:(nchar(sha)/8) ) 
    str <- append( str, base::substr( sha, 8*i - 7, 8*i ) )  
  
  sha_fmt <- paste( str, collapse = "-")
  
  
  # -- return ID
  
  # - default
  if ( is.null(type) || ( utils::head( base::tolower(type), n = 1 ) == "action" ) )
    return(sha_fmt)
  
  
  # - when UUID  
  if ( utils::head( base::tolower(type), n = 1 ) %in% c(  "uuid", "job" ) )
    return(refid)

  
  # - when SHA-1  
  if ( utils::head( base::tolower(type), n = 1 ) %in% c( "sha", "raw" ) )
    return(sha)
  

  # - otherwise  
  return(sha_fmt)
  
}




