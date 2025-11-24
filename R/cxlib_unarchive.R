#' Utility function to extract a Zip archive and verify integrity of results
#' 
#' @param x Zip archive
#' @param extract.dir Location to extract files
#' 
#' @return Vector of extracted files
#' 
#' @description
#' Unpack and extract a Zip archive `x` to directory `extract.dir` where the 
#' root of the Zip file corresponds to the root of the `extract.dir` directory. 
#' 
#' A Zip archive is a standard zip file (\link[zip]{zip}) that includes 
#' compressed files, any additional empty directories and digest files to 
#' verify archive integrity. 
#' 
#' The root of the Zip archive should include the text files `sha` and `md5` that
#' contain the SHA-1 and MD-5 message digests or hash values, respectively, for
#' each file in the archive. The digest/hash is assumed to represent the source
#' file digest/hash.
#' 
#' The format of the digest/hash file is one entry per file on individual lines
#' and in the format `<digest/hash>  <file>` (note two spaces between the 
#' digest/hash and file relative path).
#' 
#' Note that the Zip archive is first extracted to the R session temporary 
#' directory (\link[base]{tempdir}) to verify the integrity of extracted files 
#' before each file being copied to `extract.dir`. If an extracted file from
#' the archive exists in `extract.dir`, it will be overwritten. 
#' 
#' @export

cxlib_unarchive <- function( x, extract.dir = "." ) {

    
  if ( missing(x) || ! inherits(x, "character") || (length(x) != 1) || (base::trimws(x) == "") )
    stop( "Zip archive file not specified or an invalid value" )
  
  if ( ! file.exists(x) )
    stop( "The Zip archive does not exist" )

  
  if ( ! inherits(extract.dir, "character") || (length(extract.dir) != 1) || (base::trimws(extract.dir) == "") ||
       ! dir.exists(extract.dir) )
    stop( "Zip archive extract target directory not specified, an invalid value or does not exist" )
  
  
  # -- standardize archive path
  xpath_arch <- cxapp::cxapp_standardpath(x)
  
  
  # -- standardize extract directory path
  xpath_extdir <- cxapp::cxapp_standardpath(extract.dir)
  
  
  # -- retrieve list of archive entries
  lst_contents <- try( zip::zip_list( xpath_arch ), silent = TRUE )

  
  if ( inherits( lst_contents, "try-error" ) )
    stop( "Could not read archive file ", xpath_arch )
  
  lst_architems <- lst_contents[, "filename" ]
  
  
  if ( length(lst_architems) == 0 )
    return(invisible(character(0)))

    
  # -- temporary working directory
  xpath_temp <- cxapp::cxapp_standardpath( base::tempfile( pattern = "unarchive-", tmpdir = base::tempdir(), fileext = "-work-area" ) )
  
  on.exit( { base::unlink( xpath_temp, recursive = TRUE, force = TRUE ) }, add = TRUE )

  if ( ! dir.exists(xpath_temp) && ! dir.create( xpath_temp, recursive = TRUE) )
    stop( "Could not create temporary working area" )


  # -- extract files
  #    note: within archive, folders/directories end in /
  
  lst_archfiles <- lst_architems[ ! grepl( ".*/$", lst_architems ) ]

  if ( all( ! c( "sha", "md5") %in% lst_archfiles ) )
    stop( "SHA-1 or MD-5 digests for archive not avialble" )
  

  # - create parent directory structure
  for ( xpath_entry in base::unique(base::dirname(lst_archfiles)) ) 
    if ( ! dir.exists( file.path( xpath_temp, xpath_entry, fsep = "/" ) ) &&
         ! dir.create( file.path( xpath_temp, xpath_entry, fsep = "/" ), recursive = TRUE ) )
      stop( "Could not create parent directory for an file entry" )
    
  # - extract files
  ext_files <- try( zip::unzip( xpath_arch, file = lst_archfiles, overwrite = TRUE, exdir = xpath_temp ) )

  if ( inherits( ext_files, "try-error") )
    stop( "Extracting files failed" )
  
  
  # - integrity check
  if ( ! file.exists( file.path( xpath_temp, "sha", fsep = "/" ) ) &&  
       ! file.exists( file.path( xpath_temp, "md5", fsep = "/" ) ) )
    stop( "SHA-1 or MD-5 digests for archive could not be found" )
  

  # - SHA-1 integrity check
  
  lst_shadigest <- character(0)
  
  if ( file.exists( file.path( xpath_temp, "sha", fsep = "/" ) ) ) {

    for ( xentry in base::readLines( file.path( xpath_temp, "sha", fsep = "/" ), warn = FALSE ) )
      lst_shadigest[ gsub( "^(.*)\\s{2}(.*)$", "\\2", xentry ) ] <- gsub( "^(.*)\\s{2}(.*)$", "\\1", xentry )
    
    for ( xfile in base::names(lst_shadigest) )
      if ( digest::digest( file.path( xpath_temp, xfile, fsep = "/"), algo = "sha1", file = TRUE ) != lst_shadigest[ xfile ] )
        stop( "SHA-1 integrity check failed for an archive file" )
    
  } 
       

  # - MD-5 integrity check (secondary strategy)
  
  lst_md5digest <- character(0)
  
  if ( file.exists( file.path( xpath_temp, "md5", fsep = "/" ) ) ) {
    
    for ( xentry in base::readLines( file.path( xpath_temp, "md5", fsep = "/" ), warn = FALSE ) )
      lst_md5digest[ gsub( "^(.*)\\s{2}(.*)$", "\\2", xentry ) ] <- gsub( "^(.*)\\s{2}(.*)$", "\\1", xentry )
    
    for ( xfile in base::names(lst_md5digest) )
      if ( digest::digest( file.path( xpath_temp, xfile, fsep = "/"), algo = "md5", file = TRUE ) != lst_md5digest[ xfile ] )
        stop( "MD-5 integrity check failed for an archive file" )

  }
 
  
  # -- copy files to target directory
  
  lst_extracted <- character(0)
  
  for ( xfile in lst_archfiles[ ! lst_archfiles %in% c( "sha", "md5") ] ) {
    
    xpath_src <- file.path( xpath_temp, xfile, fsep = "/" )
    xpath_trgt <- file.path( xpath_extdir, xfile, fsep = "/" )
 
    if ( ! dir.exists(base::dirname(xpath_trgt)) && ! dir.create(base::dirname(xpath_trgt), recursive = TRUE) )
      stop( "Could not create parent directory for ", xpath_trgt )

    if ( ! file.copy( xpath_src, xpath_trgt, overwrite = TRUE, copy.mode = FALSE, copy.date = TRUE ) )
      stop( "Feiled to save ", xfile, " in target directory" )

        
    if ( ( length(lst_shadigest) > 0 ) &&
         ( digest::digest( xpath_trgt, algo = "sha1", file = TRUE ) != lst_shadigest[ xfile ] ) )
      stop( "SHA-1 integrity check failure for ", xpath_trgt )
    
    if ( ( length(lst_md5digest) > 0 ) &&
         ( digest::digest( xpath_trgt, algo = "md5", file = TRUE ) != lst_md5digest[ xfile ] ) )
      stop( "MD-5 integrity check failure for ", xpath_trgt )
    
    
    lst_extracted <- append( lst_extracted, xfile )
    
    xpath_src <- NULL
    xpath_trgt <- NULL
    
  }
    
  
  
  # -- create empty directories from archive

  #    note: -1 since directory reference in a zip file is an entry ending in /
  lst_archdirs <- base::unlist(lapply( lst_architems[ grepl( ".*/$", lst_architems ) ], function(x) {
    base::substr( base::trimws(x), 1, base::nchar(base::trimws(x)) - 1 )
  }))

    
  if ( length(lst_archdirs) > 0 )
    for ( xdir in lst_archdirs ) 
      if ( ! dir.exists( file.path( xpath_extdir, xdir, fsep = "/" ) ) &&
           ! dir.create( file.path( xpath_extdir, xdir, fsep = "/" ), recursive = TRUE ) )
        stop( "Could not create empty directory from archive" )
      


  # -- return list of extracted files
  return(invisible(base::sort(lst_extracted)))
}

