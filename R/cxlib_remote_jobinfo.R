#' (Experimental) Utility function presenting job information 
#' 
#' @param x Job reference
#' 
#' @return Invisible job reference
#' 
#' @description
#' The utility function displays information about the job
#' 
#' 
#' @export

cxlib_remote_jobinfo <- function(x) {
  
  
  if ( is.null(x) || any(is.na(x)) || ! inherits( x, "character" ) || (length(x) == 0) ||
       ! "context" %in% names(attributes(x)) || ( attributes(x)[["context"]] != "rcx.jobreference") ||
       ! "url" %in% names(attributes(x)) ) 
    stop( "Job reference missing or invalid" )
  

  
  # -- display job reference
  
  lst <- character(0)
  
  # - job label 
  if ( "label" %in% names(attributes(x)) )
    lst <- append( lst, c( attributes(x)[["label"]], paste(rep_len( "-", 45 ), collapse = "") ) )
  
  
  # - job ID
  lst <- append(lst, x)
  
  
  # - process queue URL
  
  if ( "url" %in% names(attributes(x)) )
    lst <- append( lst, 
                   paste0( "(URL ", attributes(x)[["url"]]  , ")") )

  
  
  cat( c( " ", 
          lst,
          " "), sep = "\n" )


  return(invisible(x))
}