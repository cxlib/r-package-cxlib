#
#
# Tests for cxlib archive
#
#


#' @cx.testsfor cxlib::cxlib_archive()


testthat::test_that( "archive.noParms", {

  #' @cx.tests Not specifying an archive file results in an error
    
  # -- test
  testthat::expect_error( cxlib::cxlib_archive(), regexp = "^Zip archive file not specified or an invalid value$" )

})



testthat::test_that( "archive.archiveFileNull", {
  
  #' @cx.tests Specifying an archive file equal to NULL results in an error
  
  # -- test
  testthat::expect_error( cxlib::cxlib_archive( NULL ), regexp = "^Zip archive file not specified or an invalid value$" )
  
})


testthat::test_that( "archive.archiveFileNA", {
  
  #' @cx.tests Specifying an archive file equal to NA results in an error
  
  # -- test
  testthat::expect_error( cxlib::cxlib_archive( NA ), regexp = "^Zip archive file not specified or an invalid value$" )
  
})



testthat::test_that( "archive.archiveFileEmptyString", {
  
  #' @cx.tests Specifying an archive file equal as an empty string results in an error
  
  # -- test
  testthat::expect_error( cxlib::cxlib_archive( "  " ), regexp = "^Zip archive file not specified or an invalid value$" )
  
})




testthat::test_that( "archive.archiveFileParentDirectoryNotExist", {
  
  #' @cx.tests Specifying an archive file whose parent directory does not exist results in an error
  
  
  # -- stage

  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")

  
  # - test file path
  
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", 
                                                             tmpdir = base::tempfile( pattern = "test-archive-parent-", tmpdir = test_root, fileext = "" ), 
                                                             fileext = ".zip" ) )

  if ( dir.exists(base::dirname(test_archive)) )
    testthat::fail( "Unexpected parent directory to test archive exists" )
  

  # -- test
  testthat::expect_error( cxlib::cxlib_archive( test_archive ), regexp = "^Parent directory for the Zip archive does not exist$" )
  
})





testthat::test_that( "archive.archiveRootNull", {
  
  #' @cx.tests Specifying root directory equal to NULL results in an error
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test file path
  
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", 
                                                             tmpdir = base::tempfile( pattern = "test-archive-parent-", tmpdir = test_root, fileext = "" ), 
                                                             fileext = ".zip" ) )
  
  if ( dir.exists(base::dirname(test_archive)) || ! dir.create( base::dirname(test_archive), recursive = TRUE) )
    testthat::fail( "Could not stage parent directory to test archive" )
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_archive( test_archive, root = NULL ), regexp = "^Zip archive root directory not specified, an invalid value or does not exist$" )
  
})







testthat::test_that( "archive.archiveRootNA", {
  
  #' @cx.tests Specifying root directory equal to NA results in an error
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test file path
  
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", 
                                                             tmpdir = base::tempfile( pattern = "test-archive-parent-", tmpdir = test_root, fileext = "" ), 
                                                             fileext = ".zip" ) )
  
  if ( dir.exists(base::dirname(test_archive)) || ! dir.create( base::dirname(test_archive), recursive = TRUE) )
    testthat::fail( "Could not stage parent directory to test archive" )
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_archive( test_archive, root = NA ), regexp = "^Zip archive root directory not specified, an invalid value or does not exist$" )
  
})




testthat::test_that( "archive.archiveRootEmptyString", {
  
  #' @cx.tests Specifying root directory as an empty string results in an error
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  # - test file path
  
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", 
                                                             tmpdir = base::tempfile( pattern = "test-archive-parent-", tmpdir = test_root, fileext = "" ), 
                                                             fileext = ".zip" ) )
  
  if ( dir.exists(base::dirname(test_archive)) || ! dir.create( base::dirname(test_archive), recursive = TRUE) )
    testthat::fail( "Could not stage parent directory to test archive" )
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_archive( test_archive, root = "  " ), regexp = "^Zip archive root directory not specified, an invalid value or does not exist$" )
  
})



testthat::test_that( "archive.archiveRootEmptyString", {
  
  #' @cx.tests Specifying root directory that does not exist results in an error
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  

  # - test file path
  
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", 
                                                             tmpdir = base::tempfile( pattern = "test-archive-parent-", tmpdir = test_root, fileext = "" ), 
                                                             fileext = ".zip" ) )
  
  if ( dir.exists(base::dirname(test_archive)) || ! dir.create( base::dirname(test_archive), recursive = TRUE) )
    testthat::fail( "Could not stage parent directory to test archive" )

  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_archroot) )
    testthat::fail( "Unexpected test archive root directory exists" )
    
  
  # -- test
  testthat::expect_error( cxlib::cxlib_archive( test_archive, root = test_archroot ), regexp = "^Zip archive root directory not specified, an invalid value or does not exist$" )
  
})





testthat::test_that( "archive.archiveAllFiles", {
  
  #' @cx.tests Create an archive with all files in a root directory
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  

  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_archroot) )
    testthat::fail( "Unexpected test archive root directory exists" )
  

  # - test files
  
  test_filenames <- replicate( 10, 
                               paste0( paste( replicate( 4, 
                                                         paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                                         simplify = TRUE ), collapse = "/" ),
                                       ".txt" ),
                               simplify = TRUE )
  
  for ( xfile in test_filenames ) {
    
    xpath <- file.path( test_archroot, xfile, fsep = "/" )
    
    if ( dir.exists( base::dirname(xpath) ) || ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage test file parent" )

    base::writeLines( paste( sample(base::letters, sample(50:150, 1), replace = TRUE), collapse = ""), con = xpath )
    
  }
  

  
  # - test archive file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
      
  # -- test
  result <- cxlib::cxlib_archive( test_archive, root = test_archroot )
  

  # -- expected
  
  expected_files <- list.files( test_archroot, recursive = TRUE, full.names = FALSE )
  
  expected_sha <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "sha1", file = TRUE ), 
            "  ",
            x )
  }))
  
  expected_md5 <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "md5", file = TRUE ), 
            "  ",
            x )
  }))
  
  
  
    
  # -- assertions
  
  # - archive content
  result_arch <- zip::zip_list( test_archive )
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files, ".cx/sha", ".cx/md5", ".cx/sources.json" ) ) )

  # - SHA-1 hashes
  zip::unzip( test_archive, ".cx/sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, ".cx/sha", fsep = "/" ), warn = FALSE )

  testthat::expect_equal( base::sort(result_sha), base::sort(expected_sha) )
  

  # - MD-5 hashes
  zip::unzip( test_archive, ".cx/md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, ".cx/md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_md5), base::sort(expected_md5) )
  
})




testthat::test_that( "archive.archiveSelectFiles", {
  
  #' @cx.tests Create an archive with selected files in a root directory
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_archroot) )
    testthat::fail( "Unexpected test archive root directory exists" )
  
  
  # - test files
  
  test_filenames <- replicate( 10, 
                               paste0( paste( replicate( 4, 
                                                         paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                                         simplify = TRUE ), collapse = "/" ),
                                       ".txt" ),
                               simplify = TRUE )
  
  for ( xfile in test_filenames ) {
    
    xpath <- file.path( test_archroot, xfile, fsep = "/" )
    
    if ( dir.exists( base::dirname(xpath) ) || ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage test file parent" )
    
    base::writeLines( paste( sample(base::letters, sample(50:150, 1), replace = TRUE), collapse = ""), con = xpath )
    
  }
  
  
  # - test archive files
  test_files <- utils::head( test_filenames, n = 3 )
  
  
  # - test archive file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  
  # -- test
  result <- cxlib::cxlib_archive( test_archive, files = test_files, root = test_archroot )
  
  
  # -- expected
  
  expected_files <- test_files
  
  expected_sha <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "sha1", file = TRUE ), 
            "  ",
            x )
  }))
  
  expected_md5 <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "md5", file = TRUE ), 
            "  ",
            x )
  }))
  
  
  
  
  # -- assertions
  
  # - archive content
  result_arch <- zip::zip_list( test_archive )
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files, ".cx/sha", ".cx/md5", ".cx/sources.json") ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, ".cx/sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, ".cx/sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_sha), base::sort(expected_sha) )
  
  
  # - MD-5 hashes
  zip::unzip( test_archive, ".cx/md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, ".cx/md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_md5), base::sort(expected_md5) )
  
})






testthat::test_that( "archive.archiveSelectFilesIncludeEmptyDirs", {
  
  #' @cx.tests Create an archive with selected files in a root directory and specified list of empty directories 
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_archroot) )
    testthat::fail( "Unexpected test archive root directory exists" )
  
  
  # - test files
  
  test_filenames <- replicate( 10, 
                               paste0( paste( replicate( 4, 
                                                         paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                                         simplify = TRUE ), collapse = "/" ),
                                       ".txt" ),
                               simplify = TRUE )
  
  for ( xfile in test_filenames ) {
    
    xpath <- file.path( test_archroot, xfile, fsep = "/" )
    
    if ( dir.exists( base::dirname(xpath) ) || ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage test file parent" )
    
    base::writeLines( paste( sample(base::letters, sample(50:150, 1), replace = TRUE), collapse = ""), con = xpath )
    
  }

  
  # - test archive files
  test_files <- utils::head( test_filenames, n = 3 )

  
  # - test directories
  test_dirs <- replicate( 5, 
                          paste( replicate( sample(2:5, 1), 
                                            paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                            simplify = TRUE ), collapse = "/" ),
                          simplify = TRUE )
  
    
  
  # - test archive file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  
  # -- test
  result <- cxlib::cxlib_archive( test_archive, files = test_files, include.dirs = test_dirs, root = test_archroot )
  
  
  # -- expected
  
  expected_files <- test_files
  
  expected_sha <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "sha1", file = TRUE ), 
            "  ",
            x )
  }))
  
  expected_md5 <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "md5", file = TRUE ), 
            "  ",
            x )
  }))
  
  
  expected_dirs <- test_dirs
  
  
  expected_files_and_dirs <- append( expected_files, paste0( expected_dirs, "/") )
  

  
  # -- assertions
  
  # - archive content
  result_arch <- zip::zip_list( test_archive )
  
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files_and_dirs, ".cx/sha", ".cx/md5", ".cx/sources.json") ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, ".cx/sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, ".cx/sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_sha), base::sort(expected_sha) )
  
  
  # - MD-5 hashes
  zip::unzip( test_archive, ".cx/md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, ".cx/md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_md5), base::sort(expected_md5) )
  
})





testthat::test_that( "archive.archiveSelectFilesIncludeEmptyDirsSourcesNotWd", {
  
  #' @cx.tests Source manifest when an archive is created with selected files in a root directory and specified list of empty directories from a specified root directory
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_archroot) )
    testthat::fail( "Unexpected test archive root directory exists" )
  
  
  # - test files
  
  test_filenames <- replicate( 10, 
                               paste0( paste( replicate( 4, 
                                                         paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                                         simplify = TRUE ), collapse = "/" ),
                                       ".txt" ),
                               simplify = TRUE )
  
  for ( xfile in test_filenames ) {
    
    xpath <- file.path( test_archroot, xfile, fsep = "/" )
    
    if ( dir.exists( base::dirname(xpath) ) || ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage test file parent" )
    
    base::writeLines( paste( sample(base::letters, sample(50:150, 1), replace = TRUE), collapse = ""), con = xpath )
    
  }
  
  
  # - test archive files
  test_files <- utils::head( test_filenames, n = 3 )
  
  
  # - test directories
  test_dirs <- replicate( 5, 
                          paste( replicate( sample(2:5, 1), 
                                            paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                            simplify = TRUE ), collapse = "/" ),
                          simplify = TRUE )
  
  
  
  # - test archive file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  
  # -- test
  result <- cxlib::cxlib_archive( test_archive, files = test_files, include.dirs = test_dirs, root = test_archroot )
  
  
  # -- expected
  
  expected_files <- test_files
  
  expected_sha <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "sha1", file = TRUE ), 
            "  ",
            x )
  }))
  
  expected_md5 <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "md5", file = TRUE ), 
            "  ",
            x )
  }))
  
  
  expected_dirs <- test_dirs
  
  
  expected_files_and_dirs <- append( expected_files, paste0( expected_dirs, "/") )
  
  
  # - sources manifest
  expected_sources <- list( "type" = "filesystem", 
                            "path" = test_archroot, 
                            "environment" = base::unname(base::Sys.info()[["nodename"]]) )
  
  
  # -- assertions
  
  # - archive content
  result_arch <- zip::zip_list( test_archive )
  
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files_and_dirs, ".cx/sha", ".cx/md5", ".cx/sources.json") ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, ".cx/sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, ".cx/sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_sha), base::sort(expected_sha) )
  
  
  # - MD-5 hashes
  zip::unzip( test_archive, ".cx/md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, ".cx/md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_md5), base::sort(expected_md5) )

  
  # - sources
  zip::unzip( test_archive, ".cx/sources.json", exdir = test_root )
  sources_json <- jsonlite::fromJSON( file.path( test_root, ".cx/sources.json", fsep = "/" ), simplifyVector = FALSE )
  
  testthat::expect_equal( sources_json[ base::sort(base::names(sources_json)) ], expected_sources[ base::sort(base::names(expected_sources)) ] )
  
    
})







testthat::test_that( "archive.archiveSelectFilesIncludeEmptyDirsSourcesWd", {
  
  #' @cx.tests Source manifest when an archive is created with selected files and specified list of empty directories and root directory is not specified
  
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_archroot) )
    testthat::fail( "Unexpected test archive root directory exists" )
  
  
  # - test files
  
  test_filenames <- replicate( 10, 
                               paste0( paste( replicate( 4, 
                                                         paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                                         simplify = TRUE ), collapse = "/" ),
                                       ".txt" ),
                               simplify = TRUE )
  
  for ( xfile in test_filenames ) {
    
    xpath <- file.path( test_archroot, xfile, fsep = "/" )
    
    if ( dir.exists( base::dirname(xpath) ) || ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage test file parent" )
    
    base::writeLines( paste( sample(base::letters, sample(50:150, 1), replace = TRUE), collapse = ""), con = xpath )
    
  }
  
  
  # - test archive files
  test_files <- utils::head( test_filenames, n = 3 )
  
  
  # - test directories
  test_dirs <- replicate( 5, 
                          paste( replicate( sample(2:5, 1), 
                                            paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                            simplify = TRUE ), collapse = "/" ),
                          simplify = TRUE )
  
  
  
  # - test archive file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  
  # - set working directory
  
  current_wd <- base::getwd()
  
  on.exit({
    base::setwd( current_wd )
  }, add = TRUE, after = FALSE )
  
  base::setwd( test_archroot )
  
  
  
  # -- test
  result <- cxlib::cxlib_archive( test_archive, files = test_files, include.dirs = test_dirs )
  
  
  # -- expected
  
  expected_files <- test_files
  
  expected_sha <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "sha1", file = TRUE ), 
            "  ",
            x )
  }))
  
  expected_md5 <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "md5", file = TRUE ), 
            "  ",
            x )
  }))
  
  
  expected_dirs <- test_dirs
  
  
  expected_files_and_dirs <- append( expected_files, paste0( expected_dirs, "/") )
  
  
  # - sources manifest
  expected_sources <- list( "type" = "filesystem", 
                            "path" = test_archroot, 
                            "environment" = base::unname(base::Sys.info()[["nodename"]]) )
  
  
  # -- assertions
  
  # - archive content
  result_arch <- zip::zip_list( test_archive )
  
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files_and_dirs, ".cx/sha", ".cx/md5", ".cx/sources.json") ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, ".cx/sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, ".cx/sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_sha), base::sort(expected_sha) )
  
  
  # - MD-5 hashes
  zip::unzip( test_archive, ".cx/md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, ".cx/md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_md5), base::sort(expected_md5) )
  
  
  # - sources
  zip::unzip( test_archive, ".cx/sources.json", exdir = test_root )
  sources_json <- jsonlite::fromJSON( file.path( test_root, ".cx/sources.json", fsep = "/" ), simplifyVector = FALSE )
  
  testthat::expect_equal( sources_json[ base::sort(base::names(sources_json)) ], expected_sources[ base::sort(base::names(expected_sources)) ] )
  
  
})






testthat::test_that( "archive.archiveNoSelectFilesExistIncludeEmptyDirs", {
  
  #' @cx.tests Create an archive when no selected files exist and specified list of empty directories
  
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_archroot) )
    testthat::fail( "Unexpected test archive root directory exists" )
  
  
  # - test files
  
  test_filenames <- replicate( 10, 
                               paste0( paste( replicate( 4, 
                                                         paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                                         simplify = TRUE ), collapse = "/" ),
                                       ".txt" ),
                               simplify = TRUE )
  
  for ( xfile in test_filenames ) {
    
    xpath <- file.path( test_archroot, xfile, fsep = "/" )
    
    if ( dir.exists( base::dirname(xpath) ) || ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage test file parent" )
    
    # do not create files
    # base::writeLines( paste( sample(base::letters, sample(50:150, 1), replace = TRUE), collapse = ""), con = xpath )
    
  }
  
  
  # - test archive files
  test_files <- utils::head( test_filenames, n = 3 )
  
  
  # - test directories
  test_dirs <- replicate( 5, 
                          paste( replicate( sample(2:5, 1), 
                                            paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                            simplify = TRUE ), collapse = "/" ),
                          simplify = TRUE )
  
  
  
  # - test archive file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  

  # -- test
  result <- cxlib::cxlib_archive( test_archive, files = test_files, include.dirs = test_dirs, root = test_archroot )
  
  
  # -- expected
  
  expected_dirs <- test_dirs
  expected_files_and_dirs <- paste0( expected_dirs, "/") 
  
  
  # - sources manifest
  expected_sources <- list( "type" = "filesystem", 
                            "path" = test_archroot, 
                            "environment" = base::unname(base::Sys.info()[["nodename"]]) )
  
  
  # -- assertions
  
  # - archive content
  result_arch <- zip::zip_list( test_archive )
  
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files_and_dirs, ".cx/sha", ".cx/md5", ".cx/sources.json") ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, ".cx/sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, ".cx/sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_length( result_sha, 0)
  

  # - MD-5 hashes
  zip::unzip( test_archive, ".cx/md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, ".cx/md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_length( result_md5, 0)
  
  
  # - sources
  zip::unzip( test_archive, ".cx/sources.json", exdir = test_root )
  sources_json <- jsonlite::fromJSON( file.path( test_root, ".cx/sources.json", fsep = "/" ), simplifyVector = FALSE )
  
  testthat::expect_equal( sources_json[ base::sort(base::names(sources_json)) ], expected_sources[ base::sort(base::names(expected_sources)) ] )
  
  
})





testthat::test_that( "archive.archiveNoSelectFilesExistFilesMustExistIncludeEmptyDirs", {
  
  #' @cx.tests Create an archive when no selected files exist, files must exist and specified list of empty directories results in an error

  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_archroot) )
    testthat::fail( "Unexpected test archive root directory exists" )
  
  
  # - test files
  
  test_filenames <- replicate( 10, 
                               paste0( paste( replicate( 4, 
                                                         paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                                         simplify = TRUE ), collapse = "/" ),
                                       ".txt" ),
                               simplify = TRUE )
  
  for ( xfile in test_filenames ) {
    
    xpath <- file.path( test_archroot, xfile, fsep = "/" )
    
    if ( dir.exists( base::dirname(xpath) ) || ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage test file parent" )
    
    # do not create files
    # base::writeLines( paste( sample(base::letters, sample(50:150, 1), replace = TRUE), collapse = ""), con = xpath )
    
  }
  
  
  # - test archive files
  test_files <- utils::head( test_filenames, n = 3 )
  
  
  # - test directories
  test_dirs <- replicate( 5, 
                          paste( replicate( sample(2:5, 1), 
                                            paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                            simplify = TRUE ), collapse = "/" ),
                          simplify = TRUE )
  
  
  
  # - test archive file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_archive( test_archive, files = test_files, files.mustexist = TRUE, include.dirs = test_dirs, root = test_archroot ), 
                          regexp = "^One or more specified files do not exist$" )
  
  
})





testthat::test_that( "archive.archiveJustEmptyDirs", {
  
  #' @cx.tests Create an archive with no files and specified list of empty directories
  
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_archroot ) && ! dir.create( test_archroot, recursive = TRUE ) )
    testthat::fail("Could not stage test archive root directory")
  

  # - test directories
  test_dirs <- replicate( 5, 
                          paste( replicate( sample(2:5, 1), 
                                            paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                            simplify = TRUE ), collapse = "/" ),
                          simplify = TRUE )

  if ( any(dir.exists( file.path( test_archroot, test_dirs, fsep = "/") )) )
    testthat::fail( "Unexpected directory is staged" )
  
  
  # - test archive file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  
  
  # -- test
  result <- cxlib::cxlib_archive( test_archive, files = character(0), include.dirs = test_dirs, root = test_archroot )
  
  
  # -- expected
  
  expected_dirs <- test_dirs
  expected_files_and_dirs <- paste0( expected_dirs, "/") 
  
  
  # - sources manifest
  expected_sources <- list( "type" = "filesystem", 
                            "path" = test_archroot, 
                            "environment" = base::unname(base::Sys.info()[["nodename"]]) )
  
  
  # -- assertions
  
  # - archive content
  result_arch <- zip::zip_list( test_archive )
  
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files_and_dirs, ".cx/sha", ".cx/md5", ".cx/sources.json") ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, ".cx/sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, ".cx/sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_length( result_sha, 0)
  
  
  # - MD-5 hashes
  zip::unzip( test_archive, ".cx/md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, ".cx/md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_length( result_md5, 0)
  
  
  # - sources
  zip::unzip( test_archive, ".cx/sources.json", exdir = test_root )
  sources_json <- jsonlite::fromJSON( file.path( test_root, ".cx/sources.json", fsep = "/" ), simplifyVector = FALSE )
  
  testthat::expect_equal( sources_json[ base::sort(base::names(sources_json)) ], expected_sources[ base::sort(base::names(expected_sources)) ] )
  
  
})





testthat::test_that( "archive.archiveAllFilesArchiveRecursion", {
  
  #' @cx.tests Create an archive with all files in a root directory excluding the archive file
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_archroot) )
    testthat::fail( "Unexpected test archive root directory exists" )
  
  
  # - test files
  
  test_filenames <- replicate( 10, 
                               paste0( paste( replicate( 4, 
                                                         paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                                         simplify = TRUE ), collapse = "/" ),
                                       ".txt" ),
                               simplify = TRUE )
  
  for ( xfile in test_filenames ) {
    
    xpath <- file.path( test_archroot, xfile, fsep = "/" )
    
    if ( dir.exists( base::dirname(xpath) ) || ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage test file parent" )
    
    base::writeLines( paste( sample(base::letters, sample(50:150, 1), replace = TRUE), collapse = ""), con = xpath )
    
  }
  
  
  
  # - test archive file
  #   note: test archive is in the root of the directory to archive
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_archroot, fileext = ".zip" ) )
  
 
  #   note: stage a symbolic archive output file
  #   note: intentionally not a zip file .. expecting it to be overwritten with a valid zip
  base::writeLines( paste( sample(base::letters, sample(50:100, 1), replace = TRUE), collapse = ""), con = test_archive )

  if ( ! file.exists( test_archroot ) )
    testthat::fail( "Failed to stage Zip file as archive" )
  
  
  # -- test
  result <- cxlib::cxlib_archive( test_archive, root = test_archroot )
  
  
  # -- expected
  
  # noted: not very scientific that all test files end in .txt
  expected_files <- list.files( test_archroot, pattern = ".*\\.txt$", recursive = TRUE, full.names = FALSE )
  
  
  expected_sha <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "sha1", file = TRUE ), 
            "  ",
            x )
  }))
  
  expected_md5 <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "md5", file = TRUE ), 
            "  ",
            x )
  }))
  
  
  
  
  # -- assertions
  
  # - archive content
  result_arch <- zip::zip_list( test_archive )
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files, ".cx/sha", ".cx/md5", ".cx/sources.json" ) ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, ".cx/sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, ".cx/sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_sha), base::sort(expected_sha) )
  
  
  # - MD-5 hashes
  zip::unzip( test_archive, ".cx/md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, ".cx/md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_md5), base::sort(expected_md5) )
  
})




testthat::test_that( "archive.archiveAllFilesArchiveRecursionInSubdir", {
  
  #' @cx.tests Create an archive with all files in a root directory excluding the archive file in a sub-directory of the root directory
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_archroot) )
    testthat::fail( "Unexpected test archive root directory exists" )
  
  
  # - test files
  
  test_filenames <- replicate( 10, 
                               paste0( paste( replicate( 4, 
                                                         paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                                         simplify = TRUE ), collapse = "/" ),
                                       ".txt" ),
                               simplify = TRUE )
  
  for ( xfile in test_filenames ) {
    
    xpath <- file.path( test_archroot, xfile, fsep = "/" )
    
    if ( dir.exists( base::dirname(xpath) ) || ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage test file parent" )
    
    base::writeLines( paste( sample(base::letters, sample(50:150, 1), replace = TRUE), collapse = ""), con = xpath )
    
  }
  
  
  
  # - test archive file
  #   note: test archive is in the parent directory of the first test file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", 
                                                             tmpdir = file.path( test_archroot, base::dirname(utils::head( test_filenames, n = 1)), fsep = "/"), fileext = ".zip" ) )
  

  #   note: stage a symbolic archive output file
  #   note: intentionally not a zip file .. expecting it to be overwritten with a valid zip
  base::writeLines( paste( sample(base::letters, sample(50:100, 1), replace = TRUE), collapse = ""), con = test_archive )
  
  if ( ! file.exists( test_archroot ) )
    testthat::fail( "Failed to stage Zip file as archive" )
  
  
  # -- test
  result <- cxlib::cxlib_archive( test_archive, root = test_archroot )
  
  
  # -- expected
  
  # noted: not very scientific that all test files end in .txt
  expected_files <- list.files( test_archroot, pattern = ".*\\.txt$", recursive = TRUE, full.names = FALSE )
  
  
  expected_sha <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "sha1", file = TRUE ), 
            "  ",
            x )
  }))
  
  expected_md5 <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "md5", file = TRUE ), 
            "  ",
            x )
  }))
  
  
  
  
  # -- assertions
  
  # - archive content
  result_arch <- zip::zip_list( test_archive )
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files, ".cx/sha", ".cx/md5", ".cx/sources.json" ) ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, ".cx/sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, ".cx/sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_sha), base::sort(expected_sha) )
  
  
  # - MD-5 hashes
  zip::unzip( test_archive, ".cx/md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, ".cx/md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_md5), base::sort(expected_md5) )
  
})




testthat::test_that( "archive.archiveNoSelectFilesExistIncludeEmptyDirs", {
  
  #' @cx.tests Create an archive when no selected files exist and specified list of empty directories
  
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_archroot) )
    testthat::fail( "Unexpected test archive root directory exists" )
  
  
  # - test files
  
  test_filenames <- replicate( 10, 
                               paste0( paste( replicate( 4, 
                                                         paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                                         simplify = TRUE ), collapse = "/" ),
                                       ".txt" ),
                               simplify = TRUE )
  
  for ( xfile in test_filenames ) {
    
    xpath <- file.path( test_archroot, xfile, fsep = "/" )
    
    if ( dir.exists( base::dirname(xpath) ) || ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage test file parent" )
    
    # do not create files
    # base::writeLines( paste( sample(base::letters, sample(50:150, 1), replace = TRUE), collapse = ""), con = xpath )
    
  }
  
  
  # - test archive files
  test_files <- utils::head( test_filenames, n = 3 )
  
  
  # - test directories
  test_dirs <- replicate( 5, 
                          paste( replicate( sample(2:5, 1), 
                                            paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                            simplify = TRUE ), collapse = "/" ),
                          simplify = TRUE )
  
  
  
  # - test archive file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  
  
  # -- test
  result <- cxlib::cxlib_archive( test_archive, files = test_files, include.dirs = test_dirs, root = test_archroot )
  
  
  # -- expected
  
  expected_dirs <- test_dirs
  expected_files_and_dirs <- paste0( expected_dirs, "/") 
  
  
  # - sources manifest
  expected_sources <- list( "type" = "filesystem", 
                            "path" = test_archroot, 
                            "environment" = base::unname(base::Sys.info()[["nodename"]]) )
  
  
  # -- assertions
  
  # - archive content
  result_arch <- zip::zip_list( test_archive )
  
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files_and_dirs, ".cx/sha", ".cx/md5", ".cx/sources.json") ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, ".cx/sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, ".cx/sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_length( result_sha, 0)
  
  
  # - MD-5 hashes
  zip::unzip( test_archive, ".cx/md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, ".cx/md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_length( result_md5, 0)
  
  
  # - sources
  zip::unzip( test_archive, ".cx/sources.json", exdir = test_root )
  sources_json <- jsonlite::fromJSON( file.path( test_root, ".cx/sources.json", fsep = "/" ), simplifyVector = FALSE )
  
  testthat::expect_equal( sources_json[ base::sort(base::names(sources_json)) ], expected_sources[ base::sort(base::names(expected_sources)) ] )
  
  
})





testthat::test_that( "archive.archiveNoSelectFilesExistFilesMustExistIncludeEmptyDirs", {
  
  #' @cx.tests Create an archive when no selected files exist, files must exist and specified list of empty directories results in an error
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_archroot) )
    testthat::fail( "Unexpected test archive root directory exists" )
  
  
  # - test files
  
  test_filenames <- replicate( 10, 
                               paste0( paste( replicate( 4, 
                                                         paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                                         simplify = TRUE ), collapse = "/" ),
                                       ".txt" ),
                               simplify = TRUE )
  
  for ( xfile in test_filenames ) {
    
    xpath <- file.path( test_archroot, xfile, fsep = "/" )
    
    if ( dir.exists( base::dirname(xpath) ) || ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage test file parent" )
    
    # do not create files
    # base::writeLines( paste( sample(base::letters, sample(50:150, 1), replace = TRUE), collapse = ""), con = xpath )
    
  }
  
  
  # - test archive files
  test_files <- utils::head( test_filenames, n = 3 )
  
  
  # - test directories
  test_dirs <- replicate( 5, 
                          paste( replicate( sample(2:5, 1), 
                                            paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                            simplify = TRUE ), collapse = "/" ),
                          simplify = TRUE )
  
  
  
  # - test archive file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  
  
  # -- test
  testthat::expect_error( cxlib::cxlib_archive( test_archive, files = test_files, files.mustexist = TRUE, include.dirs = test_dirs, root = test_archroot ), 
                          regexp = "^One or more specified files do not exist$" )
  
  
})





testthat::test_that( "archive.archiveJustEmptyDirs", {
  
  #' @cx.tests Create an archive with no files and specified list of empty directories
  
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists( test_archroot ) && ! dir.create( test_archroot, recursive = TRUE ) )
    testthat::fail("Could not stage test archive root directory")
  
  
  # - test directories
  test_dirs <- replicate( 5, 
                          paste( replicate( sample(2:5, 1), 
                                            paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                            simplify = TRUE ), collapse = "/" ),
                          simplify = TRUE )
  
  if ( any(dir.exists( file.path( test_archroot, test_dirs, fsep = "/") )) )
    testthat::fail( "Unexpected directory is staged" )
  
  
  # - test archive file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  
  
  # -- test
  result <- cxlib::cxlib_archive( test_archive, files = character(0), include.dirs = test_dirs, root = test_archroot )
  
  
  # -- expected
  
  expected_dirs <- test_dirs
  expected_files_and_dirs <- paste0( expected_dirs, "/") 
  
  
  # - sources manifest
  expected_sources <- list( "type" = "filesystem", 
                            "path" = test_archroot, 
                            "environment" = base::unname(base::Sys.info()[["nodename"]]) )
  
  
  # -- assertions
  
  # - archive content
  result_arch <- zip::zip_list( test_archive )
  
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files_and_dirs, ".cx/sha", ".cx/md5", ".cx/sources.json") ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, ".cx/sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, ".cx/sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_length( result_sha, 0)
  
  
  # - MD-5 hashes
  zip::unzip( test_archive, ".cx/md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, ".cx/md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_length( result_md5, 0)
  
  
  # - sources
  zip::unzip( test_archive, ".cx/sources.json", exdir = test_root )
  sources_json <- jsonlite::fromJSON( file.path( test_root, ".cx/sources.json", fsep = "/" ), simplifyVector = FALSE )
  
  testthat::expect_equal( sources_json[ base::sort(base::names(sources_json)) ], expected_sources[ base::sort(base::names(expected_sources)) ] )
  
  
})





testthat::test_that( "archive.archiveAllFilesArchiveRecursion", {
  
  #' @cx.tests Create an archive with all files in a root directory excluding the archive file
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( dir.exists(test_archroot) )
    testthat::fail( "Unexpected test archive root directory exists" )
  
  
  # - test files
  
  test_filenames <- replicate( 10, 
                               paste0( paste( replicate( 4, 
                                                         paste( sample(base::letters, sample(5:15, 1), replace = TRUE), collapse = ""), 
                                                         simplify = TRUE ), collapse = "/" ),
                                       ".txt" ),
                               simplify = TRUE )
  
  for ( xfile in test_filenames ) {
    
    xpath <- file.path( test_archroot, xfile, fsep = "/" )
    
    if ( dir.exists( base::dirname(xpath) ) || ! dir.create( base::dirname(xpath), recursive = TRUE ) )
      testthat::fail( "Could not stage test file parent" )
    
    base::writeLines( paste( sample(base::letters, sample(50:150, 1), replace = TRUE), collapse = ""), con = xpath )
    
  }
  
  
  
  # - test archive file
  #   note: test archive is in the root of the directory to archive
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_archroot, fileext = ".zip" ) )
  
  
  #   note: stage a symbolic archive output file
  #   note: intentionally not a zip file .. expecting it to be overwritten with a valid zip
  base::writeLines( paste( sample(base::letters, sample(50:100, 1), replace = TRUE), collapse = ""), con = test_archive )
  
  if ( ! file.exists( test_archroot ) )
    testthat::fail( "Failed to stage Zip file as archive" )
  
  
  # -- test
  result <- cxlib::cxlib_archive( test_archive, root = test_archroot )
  
  
  # -- expected
  
  # noted: not very scientific that all test files end in .txt
  expected_files <- list.files( test_archroot, pattern = ".*\\.txt$", recursive = TRUE, full.names = FALSE )
  
  
  expected_sha <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "sha1", file = TRUE ), 
            "  ",
            x )
  }))
  
  expected_md5 <- base::unlist(lapply( expected_files, function(x) {
    paste0( digest::digest( file.path( test_archroot, x, fsep = "/" ), algo = "md5", file = TRUE ), 
            "  ",
            x )
  }))
  
  
  
  
  # -- assertions
  
  # - archive content
  result_arch <- zip::zip_list( test_archive )
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files, ".cx/sha", ".cx/md5", ".cx/sources.json" ) ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, ".cx/sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, ".cx/sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_sha), base::sort(expected_sha) )
  
  
  # - MD-5 hashes
  zip::unzip( test_archive, ".cx/md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, ".cx/md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_md5), base::sort(expected_md5) )
  
})




testthat::test_that( "archive.emptyArchive", {
  
  #' @cx.tests Create an empty archive with no files or empty directories
  
  
  # -- stage
  
  test_root <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-area-", tmpdir = base::tempdir(), fileext = "") )
  
  on.exit({
    base::unlink( test_root, recursive = TRUE, force = TRUE )
  }, add = TRUE )
  
  if ( ! dir.exists( test_root ) && ! dir.create( test_root, recursive = TRUE ) )
    testthat::fail("Could not create test area")
  
  
  
  # - archive root directory
  test_archroot <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-root-", tmpdir = test_root, fileext = "" ) )
  
  if ( ! dir.exists(test_archroot) && ! dir.create( test_archroot, recursive = TRUE) )
    testthat::fail( "Could not stage test archive root directory" )

  if ( length(list.files( test_archroot, recursive = TRUE, all.files = TRUE, include.dirs = FALSE )) > 0 )
    testhat::fail( "Unexpected files exist in test archive root directory" )
  

  # - test archive file
  test_archive <- cxapp::cxapp_standardpath( base::tempfile( pattern = "test-archive-", tmpdir = test_root, fileext = ".zip" ) )
  
  
  # -- test
  result <- cxlib::cxlib_archive( test_archive, root = test_archroot )
  
  
  # -- expected
  
  # noted: not very scientific that all test files end in .txt
  expected_files <- list.files( test_archroot, pattern = ".*\\.txt$", recursive = TRUE, full.names = FALSE )
  
  

  
  # -- assertions
  
  # - archive file
  testthat::expect_true( file.exists(test_archive) )
  
  # - archive content
  result_arch <- zip::zip_list( test_archive )
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files, ".cx/sha", ".cx/md5", ".cx/sources.json" ) ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, ".cx/sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, ".cx/sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_length( result_sha, 0)
  

  
  # - MD-5 hashes
  zip::unzip( test_archive, ".cx/md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, ".cx/md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_length( result_md5, 0)
  
})

