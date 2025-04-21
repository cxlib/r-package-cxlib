#' Internal function to list actions
#' 
#' @param x Directory path with actions
#' 
#' @return List of actions
#' 
#' @keywords internal

.cxlib_batchjob_actions <- function( x ) {

  # -- initiate list of actions 
  actions <- list()
  
  
  # -- futility .. obviously no actions
  if ( ! dir.exists(x) )
    return(invisible(x))
  
  
  # -- standard job path
  xpath <- cxlib::cxlib_standardpath( x )
  
  
  
  # -- determine action files to process
  #    note: to avoid read and write locking a file is never overwritten 
  #    note: action status sequence is .json .lck -completed.json
  #    note: deriving status is reverse order 
  
  lst_files <- base::sort( list.files( xpath, 
                                       pattern = "^\\d+-action-.*.(json|lck)$", 
                                       recursive = FALSE, include.dirs = FALSE, full.names = FALSE ) )
  
  # - action index
  # - note: index is prefix before first dash
  # - note: index is natural sort order
  action_idx <- base::sort( base::unique( gsub( "^(\\d+)-action.*", "\\1", base::basename(lst_files) ) ) )
  
  # - action files
  lst_action_files <- character(0)
  
  # - process each action
  for ( xidx in action_idx ) {
    
    # - isolate files for action xidx    
    act_files <- lst_files[ base::startsWith( lst_files, paste0( xidx, "-action-" ) ) ]
    
    # - identify latest state 
    #   note: processing sequence is
    #         (1) .json
    #         (2) .lck
    #         (3) -completed.json
    #   note: latest state is search (3) to (1)
    for ( xending in c( "-completed.json", ".lck", ".json" ) )
      if ( any( base::endsWith( act_files, xending ) ) ) {
        lst_action_files <- append( lst_action_files, 
                                    file.path( xpath, 
                                               utils::head( act_files[ base::endsWith( act_files, xending ) ], n = 1 ),
                                               fsep = "/" ) )
        break
      }
    
  } # end of for-statement for each action index
  
  
  # - process each action file
  for ( xfile in lst_action_files ) {
    
    # - open read-only binary connection to the action file    
    fstream <- base::file( description = xfile, open = "rb", blocking = FALSE )
    
    # - read the action details
    xaction <- try( jsonlite::fromJSON( fstream , simplifyDataFrame = FALSE ), silent = TRUE )
    
    # - close connection to file
    base::close( fstream )
    
    if ( inherits( xaction, "try-error" ) )
      stop( "Failed to import action ", base::basename(xfile) )
    
    
    xaction[["status"]] <- "planned"
    
    if ( tools::file_ext( xfile) == "lck" )
      xaction[["status"]] <- "executing"
    
    if ( grepl( "-completed.json$", xfile, ignore.case = TRUE, perl = TRUE) )
      xaction[["status"]] <- "completed"
    
    actions[[ length(actions) + 1 ]] <- xaction
    
  }
  

    
  return(invisible(actions))
}