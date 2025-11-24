#' Utility function to create a Zip archive with integrity checks
#' 
#' @param x Archive file to create
#' @param root Root directory for Zip archive
#' @param files Sub-select files
#' @param include.dirs Include directories
#' @param recursive Recursively process sub-directory content in `x`
#' 
#' @return The path to the archive file
#' 
#' @description
#' Create a Zip archive `x` using directory `root` as the root of the Zip file.
#' 
#' A Zip archive is a standard zip file (\link[zip]{zip}) that includes 
#' compressed files, any additional empty directories and digest files to 
#' verify archive integrity. 
#' 
#' The vector `files` includes only those files that exist and in `root` or one
#' of the sub-directories of `root` recursively. The vector `files` is 
#' independent of the value for `recursive`.
#' 
#' If `files = NULL`, the function searches for files in `root`. If 
#' `recursive = TRUE`, sub-directories of `root` is included.
#' 
#' The option `include.dirs` permits creating an empty directory structure 
#' within the Zip archive. The directories do not have to exist under `root`.
#'  
#' All paths within the Zip archive are relative to the root of the archive.
#' 
#' The root of the Zip archive will include the text files `sha` and `md5` that
#' contain the SHA-1 and MD-5 message digests or hash values, respectively, for
#' each file in the archive. The digest/hash is derived on the source file
#' before adding the file to the archive, i.e. digest/hash at source.
#' 
#' The format of the digest/hash file is one entry per file on individual lines
#' and in the format `<digest/hash>  <file>` (note two spaces between the 
#' digest/hash and file relative path).
#' 
#' Note that there is a theoretical time gap between a file being added to the
#' archive and when the file digest/hash was derived. The time gap is dependent
#' on the archive size as adding large files to the archive will take longer 
#' than smaller files and thus the time between the digest/hash being derived 
#' and the file being added to the archive is longer for larger files.
#' 
#' Note that the Zip archive is first created in the R session temporary 
#' directory (\link[base]{tempdir}) before being copied to `x`. If the archive
#' file `x` exists, it will be overwritten.
#' 
#' @export

cxlib_archive <- function( x, root = ".", files = NULL, include.dirs = NULL, recursive = TRUE ) {
  
  
  if ( missing(x) || ! inherits(x, "character") || (length(x) != 1) || (base::trimws(x) == "") )
    stop( "Zip archive file not specified or an invalid value" )
  

  if ( ! dir.exists(base::dirname(x)) )
    stop( "Parent directory for the Zip archive does not exist" )
  
  
  if ( ! inherits(root, "character") || (length(x) != 1) || (base::trimws(x) == "") ||
       ! dir.exists(root) )
    stop( "Zip archive root directory not specified, an invalid value or does not exist" )
  
  
  # -- create a temporary work area
  xpath_wa <- cxapp::cxapp_standardpath( base::tempfile( pattern = "archive-", tmpdir = base::tempdir(), fileext = "-work-area" ))
  
  on.exit( { base::unlink( xpath_wa, recursive = TRUE, force = TRUE ) }, add = TRUE)
  
  if ( dir.exists(xpath_wa) || ! dir.create(xpath_wa, recursive = TRUE) )
    stop( "Could not create a temporary work area for the archive" )
  
  
  # -- standardize output path
  xpath_out <- cxapp::cxapp_standardpath(x)
  
  # -- standardize root path
  xpath_root <- cxapp::cxapp_standardpath(root)
  
  
  # -- derive files filter
  lst_files <- character(0)
  

  # - select files
  
  if ( inherits( files, "character") && (length(files) > 0) )
    for ( xentry in as.character(files) ) {
      
      # - futility if file entry is not relative or absolute path to a file 
      if ( ! inherits( xentry, "character") || ( base::trimws(xentry) == "" ) ||
           ( ! file.exists( xentry ) && 
             ! file.exists( file.path( xpath_root, cxapp::cxapp_standardpath(xentry), fsep = "/" ) ) ) )
        next()
      
      # - absolute paths
      if ( base::startsWith( xentry, "/") ) {
        
        # ignore if file entry is not under root
        if ( ! base::startsWith( cxapp::cxapp_standardpath(xentry), paste0( xpath_root, "/" ) ) )
          next()
        
        #  note: the +2 is to 
        lst_files <- append( lst_files, 
                              base::substring( cxapp::cxapp_standardpath(xentry), base::nchar(xpath_root) + 2 ) )
        
        next()
      } # end of if-statement for absolute paths
        
      # - relative path
      lst_files <- append( lst_files, cxapp::cxapp_standardpath(xentry) )
      
    }  # end of for-statement for entries in files
  
  
  # - all files
  if ( is.null(files) )
    lst_files <- cxapp::cxapp_standardpath( list.files( path = xpath_root, recursive = recursive, full.names = FALSE, include.dirs = FALSE ) )
  
  
  # -- generate hashes
  
  # - SHA-1 
  lst_sha <- base::unlist(lapply( lst_files, function(x) {
    paste0( digest::digest( file.path( xpath_root, x, fsep = "/"), algo = "sha1", file = TRUE ),
            "  ",
            x )
  }))
  
  
  if ( inherits( try( base::writeLines( lst_sha, con = file.path( xpath_wa, "sha", fsep = "/" ) ), silent = FALSE ), "try-error" ) )
    stop( "Could not create SHA-1 digest reference" )
  
  
  
  # - MD-5
  lst_md5 <- base::unlist(lapply( lst_files, function(x) {
    paste0( digest::digest( file.path( xpath_root, x, fsep = "/"), algo = "md5", file = TRUE ),
            "  ",
            x )
  }))

  if ( inherits( try( base::writeLines( lst_md5,  con = file.path( xpath_wa, "md5", fsep = "/" ) ), silent = FALSE ), "try-error" ) )
    stop( "Could not create MD-5 digest reference" )
  
    
  
  # -- create archive
  
  # - temporary archive file
  xpath_arch <- file.path( xpath_wa, base::basename(xpath_out), fsep = "/" )

  
  # - add files
  arch_file <- try( zip::zip( xpath_arch, lst_files, root = xpath_root, mode = "mirror" ), silent = FALSE )
  
  if ( inherits( arch_file, "try-error" ) )
    stop( "Failed to add files to archive" )

    
  # - add digests
  arch_file <- try( zip::zip_append( xpath_arch, c( "sha", "md5"), root = xpath_wa, recurse = FALSE, include_directories = FALSE, mode = "cherry-pick" ), silent = FALSE )
  
  if ( inherits( arch_file, "try-error" ) )
    stop( "Failed to create archive" )
  
  

  # - empty directories
  #   note: empty directories are additional dirs that do not have files
  
  lst_archdirs <- character(0)
  
  if ( inherits( include.dirs, "character") && (length(include.dirs) > 0) )
    lst_archdirs <- include.dirs[ ! include.dirs %in% base::dirname(lst_files) ]  

  if ( length(lst_archdirs) > 0 ) {
    
    src_tree <- cxapp::cxapp_standardpath( base::tempfile( pattern = "arch-source-tree-", tmpdir = xpath_wa, fileext = "" ) )
    
    for ( xpath_dir in lst_archdirs )
      if ( ! dir.exists( file.path( src_tree, xpath_dir, fsep = "/") ) && 
           ! dir.create( file.path( src_tree, xpath_dir, fsep = "/") , recursive = TRUE ) )
        stop( "Could not stage shadow directory" )
    
    lst_tree <- list.dirs( src_tree, recursive = TRUE, full.names = FALSE)
    lst_tree <- lst_tree[ lst_tree != "" ]
    

    # reduce lst_tree to longest path
    arch_tree <- character(0)
    
    for ( xpath in base::sort( lst_tree, decreasing = TRUE ) )
      if ( ! any(base::startsWith( arch_tree, paste0( xpath, "/" ) )) )
        arch_tree <- append(arch_tree, xpath)
    

    # add directory tree to zip file
    arch_file <- try( zip::zip_append( xpath_arch, arch_tree, root = src_tree, mode = "mirror" ), silent = FALSE )
    
    if ( inherits( arch_file, "try-error" ) ) 
      stop( "Failed to add directory structure to archive" )

  }
  


  # -- publish archive      
  file.copy( xpath_arch, xpath_out, overwrite = TRUE, copy.date = TRUE )

    
  return(invisible(xpath_out))
}
