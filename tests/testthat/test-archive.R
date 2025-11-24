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
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files, "sha", "md5") ) )

  # - SHA-1 hashes
  zip::unzip( test_archive, "sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, "sha", fsep = "/" ), warn = FALSE )

  testthat::expect_equal( base::sort(result_sha), base::sort(expected_sha) )
  

  # - MD-5 hashes
  zip::unzip( test_archive, "md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, "md5", fsep = "/" ), warn = FALSE )
  
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
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files, "sha", "md5") ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, "sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, "sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_sha), base::sort(expected_sha) )
  
  
  # - MD-5 hashes
  zip::unzip( test_archive, "md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, "md5", fsep = "/" ), warn = FALSE )
  
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
  
  testthat::expect_equal( base::sort(result_arch[, "filename"]), base::sort( c( expected_files_and_dirs, "sha", "md5") ) )
  
  # - SHA-1 hashes
  zip::unzip( test_archive, "sha", exdir = test_root )
  result_sha <- base::readLines( file.path( test_root, "sha", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_sha), base::sort(expected_sha) )
  
  
  # - MD-5 hashes
  zip::unzip( test_archive, "md5", exdir = test_root )
  result_md5 <- base::readLines( file.path( test_root, "md5", fsep = "/" ), warn = FALSE )
  
  testthat::expect_equal( base::sort(result_md5), base::sort(expected_md5) )
  
})








